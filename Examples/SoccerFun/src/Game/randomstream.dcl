definition module randomstream

/**	This module implements a number of infinite random streams.
	For predictable games, the onesStream can be used.
*/
import StdClass
import StdMaybe
import RandomExt

::	Stream a				:== [a]			// an infinite list of random values
::	P						:== Real		// 0<=x<=1.0

/**	intRandomStream
		creates a stream of random (positive and negative) Integer values. 
	realRandomStream
		creates a stream of random (positive and negative) Real values.
	boolRandomStream
		creates a stream of random Bool values.
	probabilityRandomStream
		creates a stream of random P values.
	onesStream
		creates a stream of one values.
*/
intRandomStream				:: !RandomSeed -> Stream Int
realRandomStream			:: !RandomSeed -> Stream Real
boolRandomStream			:: !RandomSeed -> Stream Bool
probabilityRandomStream		:: !RandomSeed -> Stream P
onesStream					::                Stream P

/**	nextRandom<Type> creates a next pseudo random value of given type.
*/
nextRandomInt				:: !RandomSeed -> .(!Int, !RandomSeed)
nextRandomReal				:: !RandomSeed -> .(!Real,!RandomSeed)
nextRandomBool				:: !RandomSeed -> .(!Bool,!RandomSeed)
nextRandomP					:: !RandomSeed -> .(!P,   !RandomSeed)
next1						:: !RandomSeed -> .(!P,   !RandomSeed)

/** make the random realistic, as in, small errors occur more often:
		\r -> 1.0-r^4 (zero <= r <= 1.0)
*/
makeRandomRealistic			:: !P -> P
//		\r -> 1-r^10
makeRandomRealisticSkilled	:: !P -> P

/** selectMostProbableAction randomly selects one of the elements of the list that have the same
	highest probability.
	If all probabilities are equal to zero, then Nothing is returned.
*/
selectMostProbableAction	:: ![(P,a)] !RandomSeed -> (!Maybe a,!RandomSeed)
