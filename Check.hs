{-# LANGUAGE GADTs, RankNTypes #-} 


module Check where

import Data.Char
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Compos
import Tree
import Error

checkNamesCap :: Tree a -> [Error]
checkNamesCap e =
	case e of
		TypeDef n -> checkUpper n
		DataDef n -> checkUpper n
		FuncDef n e -> errorPred (isLower $ head n) (FuncNotLowerError n)
		_ -> composFold checkNamesCap e
	where 
		checkUpper n = errorPred (isUpper $ head n) (TypeNotUpperError n)

removeNullAbs :: Tree a -> Tree a
removeNullAbs e =
	case e of
		Abs [] e -> e
		_ -> composOp removeNullAbs e

insertManyMap :: (Ord k) => [k] -> [v] -> Map k v -> Map k v
insertManyMap ks vs = Map.union (Map.fromList (zip ks vs))

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
					return $ errorPred (Set.member n env) (NameError n)
				Abs ps e' -> do
					env <- get
					put $ insertManySet ps env
					cn e'
				_ -> composFoldM cn e