module Error where

import Tree

data Error =
	  NameError Name
	| TypeNotUpperError Name
	| FuncNotLowerError Name
	deriving (Show)

errorPred :: Bool -> Error -> [Error]
errorPred b e = if b then [] else [e]