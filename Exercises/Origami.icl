module Origami

import StdEnv
import StdDebug

//	You can use this Start-function to test your functions, assuming they are called
//  sum`, prod`, flatten`, length`, reverse`, takeWhile`, maxList` (to avoid name clashes).

Start		= and
				  [
//				    sum`       [1 .. 5]                 == sum       [1 .. 5]
//				  , prod`      [1 .. 5]                 == prod      [1 .. 5]
//				  , flatten`   [[],[1],[1,2],[1,2,3]]   == flatten   [[],[1],[1,2],[1,2,3]]
//				  , length`    [1 .. 10]                == length    [1 .. 10]
//				  , reverse`   [1 .. 5]                 == reverse   [1 .. 5]
//				  , takeWhile` ((<>) 0) [1,2,3,0,4,5,6] == takeWhile ((<>) 0) [1,2,3,0,4,5,6]
//				  , maxList`   [1 .. 5]                 == maxList   [1 .. 5]
				  ]

sum` = trace_n "sum` not yet implemented" sum

prod` = trace_n "prod` not yet implemented" prod

flatten` = trace_n "flatten` not yet implemented" flatten

length` = trace_n "length` not yet implemented" length

reverse` = trace_n "reverse` not yet implemented" reverse

takeWhile` = trace_n "takeWhile` not yet implemented" takeWhile

maxList` = trace_n "maxList` not yet implemented" maxList

