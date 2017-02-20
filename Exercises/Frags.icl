implementation module Frags

import StdEnv
import StdDebug

frags			:: [a] -> [[a]]
frags xs		= frags` xs xs (length xs) (length xs)
 where
 	frags` _ _ 0 _				= [[]]
	frags` xs _ index1 0		= frags` (tl xs) (tl xs) (index1 - 1) (index1 - 1)
	frags` xs xs2 index1 index2	= [xs2] ++ frags` xs (xs2 % (0, length xs2 - 2)) index1 (index2 - 1)




Start 	= frags [2 .. 5]

// size of frags [1, 2, 3,..., n] is (0.5 * n) ^ 2 + 0.5 * n + 1

