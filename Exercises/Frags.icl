implementation module Frags

import StdEnv
import StdDebug

frags					:: [a] -> [[a]]
frags xs		= frags` xs (length xs) 0

frags`				:: [a] Int Int-> [[a]]
frags` xs size index
| index == size	= [[]]
| otherwise			= frags`` xs size index ++ frags` (tl xs) size (index + 1)

frags``				:: [a] Int Int -> [[a]]				
frags`` xs size index
| index == size - 1	= [xs]
| otherwise			= [xs] ++ frags`` (butLast xs (length xs)) size (index + 1)

butLast			:: [a] Int -> [a]
butLast xs size
| size == 1	= []
| otherwise		= [hd xs] ++ butLast (tl xs) (size - 1)

Start 	= frags [1,2,3,4,5,6,7]


// size of frags [1, 2, 3,..., n] is (0.5 * n) ^ 2 + 0.5 * n + 1

