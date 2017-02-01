implementation module Group

import StdEnv
import StdDebug

Start				= ( group isEven  [1..10]
					  , group isOdd   [1..10]
					  , group isDigit ['p4ssw0rd']
					  )

group				:: (a -> Bool) [a] -> [[a]]
group _ _ = trace_n "group not yet implemented" []
