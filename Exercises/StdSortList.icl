implementation module StdSortList

import StdEnv
import StdDebug

::  SortList a :== [a]

newSortList							:: SortList a
newSortList	= []

memberSort								:: a (SortList a) -> Bool | Eq, Ord a
memberSort _ []			= False
memberSort x [y:ys]
| x == y				= True
| otherwise				= memberSort x ys

insertSort								:: a (SortList a) -> SortList a | Ord a
insertSort x []				= [x]
insertSort x sortList
| (maximum sortList) <= x	= sortList ++ [x]
| (hd sortList) > x			= [x] ++ sortList
| otherwise					= [hd sortList] ++ insertSort x (tl sortList)


removeFirst								:: a (SortList a) -> SortList a | Eq, Ord a
removeFirst _ []		= []
removeFirst x [y:ys]
| x == y				= ys
| otherwise				= [y] ++ removeFirst x ys

removeAll								:: a (SortList a) -> SortList a | Eq, Ord a
removeAll _ []			= []
removeAll x [y:ys]
| x == y				= removeAll x ys
| otherwise				= [y] ++ removeAll x ys

elements								:: (SortList a) -> [a]
elements sortList	= sortList

count									:: (SortList a) -> Int
count sortList	= length sortList

minimum									:: (SortList a) -> a
minimum [x:_]	= x

maximum									:: (SortList a) -> a
maximum sortList	= sortList !! (length sortList - 1)

mergeSortList							:: (SortList a) (SortList a) -> SortList a | Eq, Ord a
mergeSortList list1 list2	= mergeSortList` list1 list2 (length list1) (length list2) []

mergeSortList`							:: (SortList a) (SortList a) Int Int (SortList a) -> SortList a | Eq, Ord a
mergeSortList` _ _ 0 0 mergeList					= mergeList
mergeSortList` _ list2 0 index2	mergeList			= mergeSortList` [] (tl list2) 0 (index2 - 1) (insertSort (hd list2) mergeList)
mergeSortList` list1 _ index1 0	mergeList			= mergeSortList` (tl list1) [] (index1 - 1) 0 (insertSort (hd list1) mergeList)
mergeSortList` list1 list2 index1 index2 mergeList	= mergeSortList` (tl list1) (tl list2) (index1 - 1) (index2 - 1) (insertSort (hd list1) (insertSort (hd list2) mergeList))


//	You can use Start to do unit testing of your functions
Start = mergeSortList [1, 5, 6, 8, 2, 4, 9, 5, 0, 1] [5, 6, 7, 2, 5, 9, 8, 2, 6]
