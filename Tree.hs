{-# LANGUAGE RankNTypes, KindSignatures, GADTs, InstanceSigs #-} 

module Tree where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import Data.Functor.Compose

type Name = String
type Param = String

data Expr_
data Binding_
data Def_

type Expr    = Tree Expr_
type Binding = Tree Binding_
type Def     = Tree Def_

data Tree :: * -> * where
	Var      :: Name    -> Expr
	TypeVar  :: Name    -> Expr
	ConstInt :: Integer -> Expr
	Abs      :: [Param] -> Expr   -> Expr
	App      :: Expr    -> [Expr] -> Expr
	If       :: Expr    -> Expr   -> Expr    -> Expr
	Let      :: Binding -> Expr   -> Expr
	ValBind  :: Name    -> Expr   -> Binding
	FuncDef  :: Name    -> Expr   -> Def

qshow :: (Show a) => String -> [a] -> String
qshow n as = n ++ " (" ++ concatMap (\x -> show x ++ ") (") as ++ ")"

instance Show (Tree a) where
	show (Var x)       = "Var " ++ show x
	show (TypeVar x)   = "TypeVar " ++ show x
	show (ConstInt i)  = "ConstInt " ++ show i
	show (Abs ps e)    = "Abs " ++ show ps ++ " (" ++ show e ++ ")"
	show (App e es)    = "App (" ++ show e ++ ") " ++ show es
	show (If e1 e2 e3) = qshow "If" [e1, e2, e3]
	show (Let b e) = qshow "Let" [e]
	show (FuncDef n e) = "FuncDef " ++ show n ++ " (" ++ show e ++ ")"

-- https://publications.lib.chalmers.se/records/fulltext/local_75172.pdf
class Compos t where
	compos :: Applicative f => (forall a. t a -> f (t a)) -> t c  -> f (t c)

composOp :: Compos t => (forall a. t a -> t a) -> t c -> t c
composOp f = runIdentity . compos (Identity . f)

composFold :: (Compos t, Monoid o) => (forall a. t a -> o) -> t c -> o
composFold f = getConst . compos (Const . f)

composM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t c -> m (t c)
composM f = unwrapMonad . compos (WrapMonad . f)

-- http://stackoverflow.com/questions/18294190/applicative-instance-for-monad-m-monoid-o-m-o
composFoldM :: (Compos t, Monad m, Monoid o) => (forall a. t a -> m o) -> t c -> m o
composFoldM f = liftM getConst . unwrapMonad . getCompose
                . compos (Compose . WrapMonad . constM . f)
	where constM xm = xm >>= (\x -> return $ Const x)

instance Compos Tree where
	compos f t =
		case t of
			Abs ps e    -> pure Abs     <*> pure ps <*> f e
			App e es    -> pure App     <*> f e     <*> traverse f es
			If e1 e2 e3 -> pure If      <*> f e1    <*> f e2          <*> f e3
			Let b e     -> pure Let     <*> f b     <*> f e
			ValBind n e -> pure ValBind <*> pure n  <*> f e
			FuncDef n e -> pure FuncDef <*> pure n  <*> f e
			_ -> pure t

removeNullAbs :: Tree a -> Tree a
removeNullAbs e =
	case e of
		Abs [] e -> e
		_ -> composOp removeNullAbs e

insertManyMap :: (Ord k) => [k] -> [v] -> Map k v -> Map k v
insertManyMap ks vs = Map.union (Map.fromList (zip ks vs))

data Error = NameError Name
	deriving (Show)

insertManySet :: (Ord a) => [a] -> Set a -> Set a
insertManySet xs s = Set.union s (Set.fromList xs)

checkNames :: (Tree a) -> [Error]
checkNames e = fst $ runState (cn e) Set.empty
	where
		cn :: (Tree a) -> State (Set Name) [Error]
		cn e =
			case e of
				Var n -> do
					env <- get
					return $ if Set.member n env then [] else [NameError n]
				Abs ps e' -> do
					env <- get
					put $ insertManySet ps env
					cn e'
				_ -> composFoldM cn e