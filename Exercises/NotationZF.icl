module NotationZF

import StdEnv
import StdDebug

g1			:: [a] [b] -> [(a,b)]
g1 as bs	= [(a,b) \\ a <- as, b <- bs]
// prints out every combination of elements in as and bs in tuples
// in a list

g2			:: [a] [b] -> [(a,b)]
g2 as bs = [(a,b) \\ a<-as & b<-bs]
// prints out every i-th element of as with every i-th element of bs in tuples
// in a list

g3			:: [a] [a] -> [(a,a)] | == a
g3 as bs	= [(a,b) \\ a <- as, b <- bs | a <> b]
// prints out every combination of elements in as and bs if they're
// not equal in tuples in a list

g4			:: [a] [a] -> [(a,a)] | == a
g4 as bs	= [(a,b) \\ a<-as, b<-bs | a==b]
// prints out every combination of elements in as and bs if
// they're equal in tuples in a list

g5		:: [[a]] -> [a]
g5 xss 	= [x \\ xs <- xss, x <- xs]
// prints out every element in the lists in the argument list in a list

g6		:: a [a] -> [Int] | == a
g6 a xs = [i \\ i <- [0..] & x <- xs | a == x]
// prints out the index-positions of a in xs in a list

Start = g6 3 [3, 4, 5, 3]
