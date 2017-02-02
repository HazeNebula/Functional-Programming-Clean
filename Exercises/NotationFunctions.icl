// Exercise made by Jasper van den Bogart (4781686) and Niels van Nistelrooij (nnnnnnn)

module NotationFunctions

import StdEnv
import StdDebug

// Computes the value of 1 + 5 (6).
f1 :: Int
f1 = 1 + 5

// Computes the smallest of two integers. If the integers are equal, the second one is returned.
f2 :: Int Int -> Int
f2 m n
| m < n	    = m
| otherwise = n

// Prints a string that repeats s n times.
f3 :: String Int -> String
f3 s n
| n <= 0    = ""
| otherwise = s +++ f3 s (n-1)

// Computes the gcd.
f4 :: Int Int -> Int
f4 x 0 = x
f4 x y = f4 y (x rem y)

// Computes the sum of a pair (tuple with length 2).
f5 :: (Int,Int) -> Int
f5 x = fst x + snd x

// Swaps the elements of a pair.
f6 :: (a,b) -> (b,a)
f6 (a,b) = (b,a)

// Swaps two elements of a pair and then swap them again (to their original position).
f7 :: (a,a) -> (a,a)
f7 x = f6 (f6 x)

Start = f7 ( 5, 3 )


// test test test