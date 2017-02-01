implementation module StdSortList

import StdEnv
import StdDebug

::  SortList a // define your implementation of SortList here

newSortList								:: SortList a
newSortList = trace_n "newSortList not yet implemented" (abort "SortList not yet implemented")

memberSort								:: a (SortList a) -> Bool | Eq, Ord a
memberSort _ _ = trace_n "memberSort not yet implemented" False

insertSort								:: a (SortList a) -> SortList a | Ord a
insertSort _ _ = trace_n "insertSort not yet implemented" (abort "SortList not yet implemented")

removeFirst								:: a (SortList a) -> SortList a | Eq, Ord a
removeFirst _ _ = trace_n "removeFirst not yet implemented" (abort "SortList not yet implemented")

removeAll								:: a (SortList a) -> SortList a | Eq, Ord a
removeAll _ _ = trace_n "removeAll not yet implemented" (abort "SortList not yet implemented")

elements								:: (SortList a) -> [a]
elements _ = trace_n "elements not yet implemented" []

count									:: (SortList a) -> Int
count _ = trace_n "count not yet implemented" zero

minimum									:: (SortList a) -> a
minimum _ = trace_n "minimum not yet implemented" (abort "SortList not yet implemented")

maximum									:: (SortList a) -> a
maximum _ = trace_n "maximum not yet implemented" (abort "SortList not yet implemented")

mergeSortList							:: (SortList a) (SortList a) -> SortList a | Eq, Ord a
mergeSortList _ _ = trace_n "mergeSortList not yet implemented" (abort "SortList not yet implemented")

//	You can use Start to do unit testing of your functions
Start = ""
