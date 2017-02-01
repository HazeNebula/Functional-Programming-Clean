module ScanAndIterate

import StdEnv
import StdDebug

/*
scan            :: (a -> b -> a) a [b] -> [a]
scan op r [a:x]	= [r : scan op (op r a) x]
scan op r []	= [r]

iterate         :: (a -> a) a -> [a]
iterate f x     = [x : iterate f (f x)]
 
flip            :: (a b -> c) b a -> c
flip f b a      = f a b
*/

// applicative order:
//Start = scan (+) 0 [1..5]

// normal order:
//Start = scan (+) 0 [1..5]

// applicative order:
//Start = scan (*) 1 [1..5]

// normal order:
//Start = scan (*) 1 [1..5]

//Start = take 5 (iterate (flip (^) 2) 2)

Start = take 10 (iterate (flip (/) 10) 123456)

