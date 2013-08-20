data Bool = True | False

and x y = case x of
	True -> y
	False -> False

or x y =
	case x of {
		True -> True;
		False -> y;
	};