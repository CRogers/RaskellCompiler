{-# LANGUAGE RankNTypes, KindSignatures, GADTs #-} 

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

type Name = String

type Param = String
{-
data Expr =
	  Var Name
	| TypeVar Name
	| ConstInt Integer
	| Abs [Param] Expr
	| App Expr [Expr]
	| If Expr Expr Expr
	| Let Binding Expr
	deriving (Show)

data Binding =
	  Val Name Expr
	deriving (Show)

data Def =
	  FuncDef Name Expr
	deriving (Show)
-}
data Expr_
data Binding_
data Def_

type Expr = Tree Expr_
type Binding = Tree Binding_
type Def = Tree Def_

data Tree :: * -> * where
	Var :: Name -> Expr
	TypeVar :: Name -> Expr
	ConstInt :: Integer -> Expr
	Abs :: [Param] -> Expr -> Expr
	App :: Expr -> [Expr] -> Expr
	If :: Expr -> Expr -> Expr -> Expr
	Let :: Binding -> Expr -> Expr
	ValBind :: Name -> Expr -> Binding
	FuncDef :: Name -> Expr -> Def

instance Show (Tree a) where
	show (Var x) = "Var " ++ show x
	show (TypeVar x) = "TypeVar " ++ show x
	show (ConstInt i) = "ConstInt " ++ show i
	show (Abs ps e) = "Abs " ++ show ps ++ " (" ++ show e ++ ")"
	show (App e es) = "App (" ++ show e ++ ") " ++ show es
	show (If e1 e2 e3) = "If (" ++ show e1 ++ ") (" ++ show e2 ++ ") (" ++ show e3 ++ ")"
	show _ = "foo"


newtype Comp b a = Comp { unC :: b }

class Compos t where
	compos :: (forall a. a -> m a)
	       -> (forall a b. m (a -> b) -> m a -> m b)
           -> (forall a. t a -> m (t a))
           -> t c
           -> m (t c)

composOp :: Compos t => (forall a. t a -> t a) -> t c -> t c
composOp f = runIdentity . composM (Identity . f)

composM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t c -> m (t c)
composM = compos return ap

composM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t c -> m ()
composM_ = composFold (return ()) (>>)

composMonoid :: (Compos t, Monoid o) => (forall a. t a -> o) -> t c -> o
composMonoid = composFold mempty mappend

composMPlus :: (Compos t, MonadPlus m) => (forall a. t a -> m b) -> t c -> m b
composMPlus = composFold mzero mplus

composFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b
composFold z c f = unC . compos (\_ -> Comp z) (\(Comp x) (Comp y) -> Comp (c x y)) (Comp . f)

appM :: (Monad m, Monoid o) => m o -> m o -> m o
appM xm ym = do
	x <- xm
	y <- ym
	return $ mappend x y

joinAppM :: (Monad m, Monoid o) => [m o] -> m o
joinAppM ms = foldl appM (return mempty) ms

class ComposFoldM t where
	composFoldM :: (Monad m, Monoid o) => (forall a. t a -> m o) -> t c -> m o

instance Compos Tree where
	compos return ap f t =
		case t of
			Abs ps e -> return Abs `ap` return ps `ap` f e
			--App e es -> return App `ap` f e `ap` mapM f es
			_ -> return t
		where mapM g = foldr (ap . ap (return (:)) . g) (return [])

instance ComposFoldM Tree where
	composFoldM f t =
		case t of
			Abs ps e -> f e
			App e es -> f e `appM` mapM es
			_ -> return mempty
		where
			-- mapM :: (forall a. Tree a -> m o) -> [Tree c] -> m o
			mapM es = joinAppM (map f es)

removeNullAbs :: Tree a -> Tree a
removeNullAbs e =
	case e of
		Abs [] e -> e
		_ -> composOp removeNullAbs e

insertManyMap :: (Ord k) => [k] -> [v] -> Map k v -> Map k v
insertManyMap ks vs = Map.union (Map.fromList (zip ks vs))

{-
type Env = Map Param ()

environment :: Expr -> State Env Expr
environment e =
	case e of
		Abs ps e' -> do
			env <- get
			put $ insertManyMap ps (repeat ()) env
			e'' <- environment e'
			return $ Abs ps e''
		_ -> composM environment e
-}
data Error = NameError Name
	deriving (Show)

insertManySet :: (Ord a) => [a] -> Set a -> Set a
insertManySet xs s = Set.union s (Set.fromList xs)

checkNames :: (Tree a) -> State (Set Name) [Error]
checkNames e =
	case e of
		Var n -> do
			env <- get
			return $ if Set.member n env then [] else [NameError n]
		Abs ps e' -> do
			env <- get
			put $ insertManySet ps env
			checkNames e'
		_ -> composFoldM checkNames e




--}