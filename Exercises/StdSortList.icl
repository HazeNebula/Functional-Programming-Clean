//	Exercise made by Jasper van den Bogart (4781686) and Niels van Nistelrooij (4713648)

implementation module StdSortList

import StdEnv
import StdDebug

::  SortList a = EmptySortList | SortList [a]

newSortList								:: SortList a
newSortList								= EmptySortList

memberSort								:: a (SortList a) -> Bool | Eq, Ord a
memberSort x EmptySortList				= False
memberSort x ( SortList [head : tail] )
	| x == head							= True
	| tail == []						= False
	| otherwise							= memberSort x ( SortList tail )

insertSort								:: a (SortList a) -> SortList a | Ord a
insertSort x EmptySortList				= SortList [x]
insertSort x ( SortList [head : tail] )
	| head <= x							= SortList ( [head] ++ [x] ++ tail )
	| otherwise							= insertSort x ( SortList tail )

removeFirst									:: a (SortList a) -> SortList a | Eq, Ord a
removeFirst x EmptySortList					= EmptySortList
removeFirst x ( SortList l )				= removeFirst` x [] l
	where
		removeFirst` x previous [head : tail]
			| x == head						= SortList ( previous ++ tail )
			| otherwise						= removeFirst` x ( previous ++ [head] ) tail

removeAll								:: a (SortList a) -> SortList a | Eq, Ord a
removeAll x EmptySortList				= EmptySortList
removeAll x ( SortList l )
	| memberSort x ( SortList l )		= removeAll x ( removeFirst x ( SortList l ) )
	| otherwise							= SortList l

elements								:: (SortList a) -> [a]
elements EmptySortList					= []
elements ( SortList l )					= l

count									:: (SortList a) -> Int
count EmptySortList						= 0
count l									= length ( elements l )

minimum									:: (SortList a) -> a
minimum ( EmptySortList )				= abort "SortList is empty"
minimum ( SortList l )					= l !! 0

maximum									:: (SortList a) -> a
maximum ( EmptySortList )				= abort "SortList is empty"
maximum ( SortList l )					= l !! ( length l - 1 )

mergeSortList									:: (SortList a) (SortList a) -> SortList a | Eq, Ord a
mergeSortList EmptySortList sl2											= sl2
mergeSortList sl1 EmptySortList											= sl1
mergeSortList ( SortList [] ) ( SortList l2 )							= SortList l2
mergeSortList ( SortList l1 ) ( SortList [] )							= SortList l1
mergeSortList ( SortList [head1 : tail1] ) ( SortList [head2 : tail2] )
	| head1 <= head2													= SortList ( [head1] ++ elements ( mergeSortList ( SortList tail1 ) ( SortList ( [head2] ++ tail2 ) ) ) )
	| otherwise															= SortList ( [head2] ++ elements ( mergeSortList ( SortList ( [head1] ++ tail1 ) ) ( SortList tail2 ) ) )

//	You can use Start to do unit testing of your functions
Start = mergeSortList ( SortList [1, 2, 3] ) ( SortList [3, 4, 5] )
