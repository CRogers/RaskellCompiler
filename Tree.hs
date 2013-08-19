{-# LANGUAGE KindSignatures, GADTs, StandaloneDeriving, FlexibleInstances, UndecidableInstances #-} 

module Tree where

import Control.Monad.State
import Control.Applicative
import Data.Traversable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Compos

type Name = String
type Param = String

data Expr_
data Binding_
data Def_

type Expr    = Tree Expr_
type Binding = Tree Binding_
type Def     = Tree Def_

data Tree :: * -> * where
	Var      :: Name      -> Expr
	TypeVar  :: Name      -> Expr
	ConstInt :: Integer   -> Expr
	Abs      :: [Param]   -> Expr   -> Expr
	App      :: Expr      -> [Expr] -> Expr
	If       :: Expr      -> Expr   -> Expr    -> Expr
	Let      :: [Binding] -> Expr   -> Expr
	ValBind  :: Name      -> Expr   -> Binding
	FuncDef  :: Name      -> Expr   -> Def
	TypeDef  :: Name      -> Def

deriving instance Show Expr_
deriving instance Show Binding_
deriving instance Show Def_	
deriving instance Show a => Show (Tree a)

deriving instance Eq Expr_
deriving instance Eq Binding_
deriving instance Eq Def_
deriving instance Eq a => Eq (Tree a)

instance Compos Tree where
	compos f t =
		case t of
			Abs ps e    -> pure Abs     <*> pure ps       <*> f e
			App e es    -> pure App     <*> f e           <*> traverse f es
			If e1 e2 e3 -> pure If      <*> f e1          <*> f e2  <*> f e3
			Let bs e    -> pure Let     <*> traverse f bs <*> f e
			ValBind n e -> pure ValBind <*> pure n        <*> f e
			FuncDef n e -> pure FuncDef <*> pure n        <*> f e
			TypeDef n   -> pure TypeDef <*> pure n
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