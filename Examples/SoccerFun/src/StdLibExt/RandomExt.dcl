definition module RandomExt

//	**************************************************************************************************
//
//	General utility for random number generation.
//
//	This module is actually an extension of the Random module in the 'Object IO Examples:gui utilities'
//	folder. That module lacked instances of the usual overloaded functions.
//	
//	**************************************************************************************************

import StdTime
import StdClass

::	RandomSeed

nullRandomSeed	:: RandomSeed
//	nullRandomSeed generates a useless RandomSeed (random nullRandomSeed = (0,nullRandomSeed)).

getNewRandomSeed:: !*env	-> (!RandomSeed, !*env)	| TimeEnv env
//	GetNewRandomSeed generates a useful RandomSeed, using the current time.

random			:: !RandomSeed		-> (!Int, !RandomSeed)
//	Given a RandomSeed, Random generates a random number and a new RandomSeed.

instance toString   RandomSeed
instance fromString RandomSeed
instance ==         RandomSeed
