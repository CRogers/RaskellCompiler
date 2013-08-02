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

data Expr =
	  Var Name
	| TypeVar Name
	| ConstInt Integer
	| Abs [Param] Expr
	| App Expr [Expr]
	| If Expr Expr Expr
	| Let Defn Expr
	deriving (Show)

data Defn =
	  Val Name Expr
	deriving (Show)

data Def =
	  FuncDef Name Expr
	deriving (Show)

class Compos t where
	compos :: Applicative f => (t -> f t) -> t -> f t

	composOp :: (t -> t) -> t -> t
	composOp f = runIdentity . compos (Identity . f)

	composM :: (Monad m) => (t -> m t) -> t -> m t
	composM f = unwrapMonad . compos (WrapMonad . f)

	composFold :: (Monoid o) => (t -> o) -> t -> o
	composFold f = getConst . compos (Const . f)

instance Compos Expr where
	compos f e =
		case e of
			Abs ps e -> pure Abs <*> pure ps <*> f e
			App e es -> pure App <*> f e <*> traverse f es
			_ -> pure e


removeNullAbs e =
	case e of
		Abs [] e -> e
		_ -> composOp removeNullAbs e

insertManyMap :: (Ord k) => [k] -> [v] -> Map k v -> Map k v
insertManyMap ks vs = Map.union (Map.fromList (zip ks vs))

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

data Error = NameError Name

insertManySet :: (Ord a) => [a] -> Set a -> Set a
insertManySet xs s = Set.union s (Set.fromList xs)

checkNames :: Expr -> State (Set Name) [Error]
checkNames e =
	case e of
		Var n -> do
			env <- get
			return $ if Set.member n env then [] else [NameError n]
		Abs ps e' -> do
			env <- get
			put $ insertManySet ps env
			checkNames e'
		_ -> composM checkNames e




