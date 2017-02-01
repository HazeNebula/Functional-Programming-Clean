module Fibonacci

import StdEnv
import StdDebug

N = 46

//  1.
fibonacci				:: Int -> Int
fibonacci _ = trace_n "fibonacci not yet implemented" 0

Start					= [fibonacci i \\ i <- [1..N]]

//  2.

sumLists				:: [a] [a] -> [a] | + a
sumLists _ _ = trace_n "sumLists not yet implemented" []

//  3.
/* remove comments when you've implemented sumLists:
Start					= take N fibs
where
    fibs				= [1,1 : sumLists fibs (tl fibs)]
*/

