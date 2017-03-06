// Coded by Niels van Nistelrooij (s4713648) and Jasper van den Bogart (s4781686)

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


sum` 							:: [a] -> a | zero, + a
sum` list						= foldl (+) zero list

prod`							:: [a] -> a | one, * a
prod` list						= foldl (*) one list

flatten`						:: [[a]] -> [a]
flatten` list					= foldl (++) [] list

length`							:: [a] -> Int
length` list					= foldl (\ n x	= n + 1) 0 list

reverse`						:: [a] -> [a]
reverse` list					= foldl (\ xs x	= [x:xs]) [] list

takeWhile`						:: (a -> Bool) [a] -> [a]
takeWhile` f list				= foldr (function f) [] list
	where
		function				:: (a -> Bool) a [a] -> [a]
		function f element list
		| f element				= ([element] ++ list)
		| otherwise				= []

maxList`						:: [a] -> a | zero, < a		
maxList` list					= foldl max` zero list
	where
		max`					:: a a -> a | < a
		max` maxUntilNow element
		| element < maxUntilNow	= maxUntilNow
		| otherwise				= element

