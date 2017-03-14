implementation module RandomExt

import StdEnvExt
import StdClass, StdInt, StdString
import StdTime


::	RandomSeed	= RS !Int


nullRandomSeed :: RandomSeed
nullRandomSeed
= RS 0

getNewRandomSeed :: !*env -> (!RandomSeed, !*env) | TimeEnv env
getNewRandomSeed env
# ({hours,minutes,seconds}, env)	= getCurrentTime env
= (RS (1+(hours+minutes+seconds) bitand 65535), env)

random :: !RandomSeed -> (!Int,!RandomSeed)
random (RS seed)
= (newSeed,RS newSeed)
where
	newSeed		= if (nextSeed>=0) nextSeed (nextSeed+65537)
	nextSeed	= (seed75 bitand 65535)-(seed75>>16)
	seed75		= seed*75

instance toString   RandomSeed where toString (RS r)    = toString r
instance fromString RandomSeed where fromString str     = RS (fromString str)
instance ==         RandomSeed where == (RS r1) (RS r2) = r1 == r2
