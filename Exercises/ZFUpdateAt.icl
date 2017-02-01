implementation module ZFUpdateAt

import StdEnv
import StdDebug

//	You can use this Start-function to test your implementation
Start				= updateAt2 30 42 [100,90 .. 0] == updateAt 30 42 [100,90 .. 0]

updateAt2			:: !Int a ![a] -> [a]
updateAt2 _ _ _ = trace_n "updateAt2 not yet implemented" []

