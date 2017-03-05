module Origami

import StdEnv
import StdDebug

//	You can use this Start-function to test your functions, assuming they are called
//  sum`, prod`, flatten`, length`, reverse`, takeWhile`, maxList` (to avoid name clashes).

Start		= and
				  [
				    sum`       [1 .. 5]                 == sum       [1 .. 5]
				  , prod`      [1 .. 5]                 == prod      [1 .. 5]
				  , flatten`   [[],[1],[1,2],[1,2,3]]   == flatten   [[],[1],[1,2],[1,2,3]]
				  , length`    [1 .. 10]                == length    [1 .. 10]
				  , reverse`   [1 .. 5]                 == reverse   [1 .. 5]
				  , takeWhile` ((<>) 0) [1,2,3,0,4,5,6] == takeWhile ((<>) 0) [1,2,3,0,4,5,6]
				  , maxList`   [1 .. 5]                 == maxList   [1 .. 5]
				  ]



sum` list						= foldl (+) zero list

prod` list						= foldl (*) one list

flatten` list					= foldl (++) [] list

length` list					= foldl (\ n x	= n + 1) 0 list

reverse` list					= foldl (\ xs x	= [x:xs]) [] list

takeWhile` f list				= foldr (function f) [] list
	where
		function				:: (a -> Bool) a [a] -> [a]
		function f element list
		| f element				= ([element] ++ list)
		| otherwise				= []

maxList` list	= foldl max` zero list
	where
		max`					:: a a -> a | Ord a
		max` maxUntilNow element
		| element < maxUntilNow	= maxUntilNow
		| otherwise				= element

