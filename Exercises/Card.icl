implementation module Card

import StdEnv
import StdDebug

Start = "replace this with unit tests"

//	1.
// Define Card, Suit, Value here:

//	2.
instance == Card where
	== _ _ = trace_n "instance == Card not yet implemented" False

//	3.
instance toString Card where
	toString _ = trace_n "instance toString Card not yet implemented" ""

instance fromString Card where
	fromString _ = abort "instance fromString Card not yet implemented"

