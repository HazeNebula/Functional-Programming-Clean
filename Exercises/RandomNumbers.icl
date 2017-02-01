implementation module RandomNumbers

import StdEnv
import StdDebug
import Random

Start			:: *World -> ([Int],*World)
Start world
# (rs,world)	= getNewRandomSeed world
= (shuffle [1..10] rs,world)

random_n		:: Int RandomSeed -> ([Int],RandomSeed)
random_n _ rs = trace_n "random_n not yet implemented" ([],rs)

random_inf		:: RandomSeed -> [Int]
random_inf _ = trace_n "random_inf not yet implemented" []

iterateSt		:: (s -> (a,s)) s -> [a]
iterateSt _ _ = trace_n "iterateSt not yet implemented" []

shuffle			:: [a] RandomSeed -> [a]
shuffle _ _ = trace_n "shuffle not yet implemented" []

