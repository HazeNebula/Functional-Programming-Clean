implementation module ZFRemoveAt

import StdEnv
import StdDebug

//	You can use this Start-function to test your implementation
Start			= removeAt 42 [1 .. 100] == removeAt2 42 [1 .. 100]

removeAt2		:: Int [a] -> [a]
removeAt2 _ _ = trace_n "removeAt2 not yet implemented" []

