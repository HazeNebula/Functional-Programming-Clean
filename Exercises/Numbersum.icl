module Numbersum

import StdEnv
import StdDebug

//	This should return True
Start			= numbersum 9876543 == 42 &&
				  numbersum 1000    == 1

numbersum		:: Int -> Int
numbersum _ = trace_n "numbersum not yet implemented" 0

