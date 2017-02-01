module NotationFunctions

import StdEnv
import StdDebug

f1 :: Int
f1 = 1 + 5

f2 :: Int Int -> Int
f2 m n
| m < n	    = m
| otherwise = n

f3 :: String Int -> String
f3 s n
| n <= 0    = ""
| otherwise = s +++ f3 s (n-1)

f4 :: Int Int -> Int
f4 x 0 = x
f4 x y = f4 y (x rem y)

f5 :: (Int,Int) -> Int
f5 x = fst x + snd x

f6 :: (a,b) -> (b,a)
f6 (a,b) = (b,a)

f7 :: (a,a) -> (a,a)
f7 x = f6 (f6 x)

Start = f1
