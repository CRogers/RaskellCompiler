module PrettyPrint where

data PrettyInstr = Indent | Outdent | Newline | Text String
	deriving (Show, Eq)

class PrettyPrint a where
	pretty :: a -> [PrettyInstr]

evalPretty :: [PrettyInstr] -> String
evalPretty ps = ep ps 0
	where
		ep [] i = ""
		ep (p:ps) i = case p of
			Indent  -> ep ps (i+1)
			Outdent -> ep ps (i-1)
			Newline -> take i (repeat ' ') ++ ep ps i
			Text s  -> s ++ ep ps i