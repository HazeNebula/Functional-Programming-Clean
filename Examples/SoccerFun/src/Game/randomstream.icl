implementation module randomstream

import StdEnv, StdEnvExt
import RandomExt

::	Stream a :== [a]

randomStream					:: !Int !([Int] -> a) !RandomSeed -> Stream a
randomStream nrInts transform seed
	# (ints,seed)				= randoms nrInts seed
	= [transform ints : randomStream nrInts transform seed]
where
	randoms						:: !Int !RandomSeed -> (![Int],!RandomSeed)
	randoms 0 seed				= ([],seed)
	randoms n seed
		# (r, seed)				= random seed
		# (rs,seed)				= randoms (n-1) seed
		= ([r:rs],seed)

intRandomStream					:: !RandomSeed -> Stream Int
intRandomStream seed			= randomStream 2 createInt seed
where
	createInt					:: ![Int] -> Int
	createInt [a,b]				= if (isOdd a) -1 1 * (abs b)
	createInt _					= abort "Fatal error: intRandomStream applied to unexpected number of integers.\n"

realRandomStream				:: !RandomSeed -> Stream Real
realRandomStream seed			= randomStream 4 createReal seed
where
	createReal					:: ![Int] -> Real
	createReal [s,a,b,c]		= toReal (if (isOdd s) "-" "" <+++ abs a <+++ "." <+++ abs b <+++ "E" <+++ (c rem 100))
	createReal _				= abort "Fatal error: realRandomStream applied to unexpected number of integers.\n"

boolRandomStream				:: !RandomSeed -> Stream Bool
boolRandomStream seed			= randomStream 1 createBool seed
where
	createBool					:: ![Int] -> Bool
	createBool [i]				= isEven i
	createBool _				= abort "Fatal error: boolRandomStream applied to unexpected number of integers.\n"

probabilityRandomStream			:: !RandomSeed -> Stream P
probabilityRandomStream seed	= randomStream 2 createProbability seed
where
	createProbability			:: ![Int] -> P
	createProbability [a,b]
		| b==0					= 1.0
		| otherwise				= (toReal a`) / (toReal b`)
	where
		[a`,b`:_]				= sort [abs a,abs b]
	createProbability _			= abort "Fatal error: probabilityRandomStream applied to unexpected number of integers.\n"

onesStream						:: Stream P
onesStream						= [one,one..]

nextRandomInt					:: !RandomSeed -> .(!Int, !RandomSeed)
nextRandomInt seed
	# (r1,seed)					= random seed
	# (r2,seed)					= random seed
	= (if (isOdd r1) -1 1 * (abs r2),seed)

nextRandomReal					:: !RandomSeed -> .(!Real,!RandomSeed)
nextRandomReal seed
	# (r1,seed)					= random seed
	# (r2,seed)					= random seed
	# (r3,seed)					= random seed
	# (r4,seed)					= random seed
	= (toReal (if (isOdd r1) "-" "" <+++ abs r2 <+++ "." <+++ abs r3 <+++ "E" <+++ (r4 rem 100)),seed)
	
nextRandomBool					:: !RandomSeed -> .(!Bool,!RandomSeed)
nextRandomBool seed
	# (r1,seed)					= random seed
	= (isEven r1,seed)

nextRandomP						:: !RandomSeed -> .(!P,   !RandomSeed)
nextRandomP seed
	# (r1,seed)					= random seed
	# (r2,seed)					= random seed
	# (a,b)						= minmax (abs r1,abs r2)
	= (if (r2==0) 1.0 ((toReal a)/(toReal b)),seed)

next1							:: !RandomSeed -> .(!P,   !RandomSeed)
next1 seed						= (1.0,seed)

/** make the random realistic, as in, small errors occur more often:
		\r -> 1.0-r^4
		zero <= r <= 1.0
*/
makeRandomRealistic				:: !P -> P
makeRandomRealistic r			= 1.0-r^4.0

makeRandomRealisticSkilled		:: !P -> P
makeRandomRealisticSkilled r	= 1.0-r^10.0

selectMostProbableAction		:: ![(P,a)] !RandomSeed -> (!Maybe a,!RandomSeed)
selectMostProbableAction odds seed
	= case sortBy (\(p1,_) (p2,_) -> p1 > p2) (filter (\(p,_) -> p > zero) odds) of
		[]						= (Nothing,seed)
		odds=:[(p,_):_]			= let mostProbable	= takeWhile ((==) p o fst) odds
								      nr			= length mostProbable
								      (p1,seed1)	= nextRandomP seed
								   in (Just (snd (odds !! (entier (p1 * toReal nr)))),seed1)
