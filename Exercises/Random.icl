implementation module Random


import StdInt, StdClass


:: RandomSeed :== Int


nullRandomSeed :: RandomSeed
nullRandomSeed = 0

/// if using Windows ...
import StdTime

getNewRandomSeed :: !*World -> (!RandomSeed, !*World)
getNewRandomSeed w
# ({hours,minutes,seconds}, w) = getCurrentTime w
= (1+(hours+minutes+seconds) bitand 65535, w)
/// if using Linux ...
//getNewRandomSeed :: !*World -> (!RandomSeed, !*World)
//getNewRandomSeed w
//# (time,w) = getCurrentTime 0 w
//= (time bitand 65535,w)
//where
//	getCurrentTime :: !Int !*World -> (!Int, !*World)
//	getCurrentTime a0 w = code {
//		ccall time "I:I:p"
//	}
/// endif


random :: !RandomSeed -> .(!Int,!RandomSeed)
random seed
= (newSeed,newSeed)
where
	newSeed  = if (nextSeed>=0) nextSeed (nextSeed+65537)
	nextSeed = (seed75 bitand 65535)-(seed75>>16)
	seed75   = seed*75
