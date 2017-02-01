implementation module Firsts

import StdEnv
import StdDebug

Start					= firsts list
where
	list				= [1,2,3,4,5]

firsts					:: [a] -> [[a]]
firsts _ = trace_n "firsts not yet implemented" []

