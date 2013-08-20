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
import Util

checkNamesCap :: Tree a -> [Error]
checkNamesCap e = fst $ runState (cn e) []
	where 
		cn :: (Tree a) -> State SourceStack [Error]
		cn e =
			case e of
				Annot sp ep e -> do
					stack <- get
					put $ SourceSect sp ep : stack
					cn e
				TypeDef n -> checkUpper n
				DataDef n as -> concatMapM checkUpper (n:as) 
				FuncDef n e -> appendM (checkLower n) (cn e)
				Abs ps e -> appendM (concatMapM checkLower ps) (cn e)
				_ -> composFoldM cn e
			where
				checkCase :: (Char -> Bool) -> (Name -> ErrorDesc) -> Name -> State SourceStack [Error]
				checkCase isCase err n = do
					stack <- get
					return $ errorPred (isCase $ head n) (Error stack $ err n)

				checkUpper = checkCase isUpper TypeNotUpperError
				checkLower = checkCase isLower IdentNotLowerError

removeNullAbs :: Tree a -> Tree a
removeNullAbs e =
	case e of
		Abs [] e -> e
		_ -> composOp removeNullAbs e

insertManyMap :: (Ord k) => [k] -> [v] -> Map k v -> Map k v
insertManyMap ks vs = Map.union (Map.fromList (zip ks vs))

insertManySet :: (Ord a) => [a] -> Set a -> Set a
insertManySet xs s = Set.union s (Set.fromList xs)
{-
checkNames :: (Tree a) -> [Error b]
checkNames e = fst $ runState (cn e) Set.empty
	where
		cn :: (Tree a) -> State (Set Name) [Error b]
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
-}