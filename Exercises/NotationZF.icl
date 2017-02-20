//	Exercise made by Jasper van den Bogart (4781686) and Niels van Nistelrooij (4713648)

module NotationZF

import StdEnv
import StdDebug

//	g1 creates a list of tuples of every element a with every element b
g1			:: [a] [b] -> [( a, b )]
g1 as bs	= [(a,b) \\ a <- as, b <- bs]

//	g2 creates a list of tuples in which every element a is paired with the element b with the same index
g2			:: [a] [b] -> [( a, b )]
g2 as bs = [(a,b) \\ a<-as & b<-bs]

//	g3 creates a list of tuples in which every element a is paired with every element b that is not equal to a
g3			:: [a] [a] -> [( a, a )] | == a
g3 as bs	= [(a,b) \\ a <- as, b <- bs | a <> b]

//	g4 creates a list of tuples in which every element a is paired with every element b that is equal to a
g4			:: [a] [a] -> [( a, a )] | == a
g4 as bs = [(a, b) \\ a<-as, b<-bs | a==b]

//	g5 takes every list inside of a list and puts the elements after each other
g5			:: [[a]] -> [a]
g5 xss = [x \\ xs <- xss, x <- xs]

//	g6 returns a list of the indices of every element in xs that is equal to a
g6			:: a [a] -> [Int] | == a
g6 a xs = [i \\ i <- [0..] & x <- xs | a == x]

Start = ( g1 [1, 2, 3] [4, 5, 6], '\n'
		, g2 [1, 2, 3] [4, 5, 6], '\n'
		, g3 [1, 2, 3] [3, 4, 5], '\n'
		, g4 [1, 2, 3] [3, 4, 5], '\n'
		, g5 [[1, 2, 3], [4, 5, 6]], '\n'
		, g6 2 [2, 4, 3, 2] )
