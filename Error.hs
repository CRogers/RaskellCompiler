{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Error where

import qualified Data.Map as Map
import Tree
import Util

class ShowError a where
	showError :: Sources -> a -> String

data Error = Error SourceStack ErrorDesc
	deriving (Show)

data ErrorDesc =
	  NameError Name
	| TypeNotUpperError Name
	| IdentNotLowerError Name
	deriving (Show)

instance ShowError Error where
	showError sources (Error stack desc) =
		   showError sources desc  ++ "\n"
		++ showError sources stack ++ "\n"

instance ShowError SourceStack where
	showError sources ss = joinL "\n" $ map (showError sources) ss

instance ShowError SourceSect where
	showError sources (SourceSect (Pos source sl sc) (Pos _ el ec)) =
		case Map.lookup source sources of
			Nothing -> error "Could not find source " ++ source ++ " in sources list: " ++ show (Map.keys sources)
			Just file ->
				   "In expression (" ++ source ++ ":" ++ show sl ++ ":" ++ show sc ++ "-"
				++ show el ++ ":" ++ show ec ++ "):\n\t"
				++ joinL "\t\n" (map (slice (sc-1) (ec-1)) $ slice (sl-1) (el-1) file)

instance ShowError ErrorDesc where
	showError sources errdesc =
		case errdesc of
			NameError n -> "Name error for " ++ n
			TypeNotUpperError n  -> "The type " ++ n ++ " should begin with an uppercase character"
			IdentNotLowerError n -> "The indentifier " ++ n ++ " should begin with an lowercase character"

errorPred :: Bool -> Error -> [Error]
errorPred b e = if b then [] else [e]