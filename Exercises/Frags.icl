//	Exercise made by Jasper van den Bogart (4781686) and Niels van Nistelrooij (4713648)

implementation module Frags

import StdEnv
import StdDebug

//	frags has type [a] -> [[a]]
//	the number of fragments given a list of n elements is 1 / 2 * ( n ^ 2 + n + 2 )

Start = frags [1..3]

frags											:: [a] -> [[a]]
frags xs										= frags` xs 1 0
	where
		frags`									:: [a] Int Int -> [[a]]
		frags` xs n start_index
			| ( start_index + n ) <= length xs	= [getfrag xs n start_index] ++ frags` xs n ( start_index + 1 )
			| n < length xs						= frags` xs ( n + 1 ) 0
			| otherwise							= [[]]
		getfrag									:: [a] Int Int -> [a]
		getfrag xs n start_index				= [xs !! ( i + start_index ) \\ i <- [0 .. ( n - 1 )]]
