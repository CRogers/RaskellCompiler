module Util where

import Control.Monad
import Data.List

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs =  liftM concat (mapM f xs)

appendM :: (Monad m) => m [a] -> m [a] -> m [a]
appendM = liftM2 (++)

joinL :: [a] -> [[a]] -> [a]
joinL delim l = concat (intersperse delim l)

slice from to xs = take (to - from + 1) (drop from xs)