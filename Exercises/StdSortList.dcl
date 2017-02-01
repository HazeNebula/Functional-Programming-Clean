definition module StdSortList

import StdClass

::  SortList a

newSortList   :: SortList a                                    // empty sorted list
memberSort    :: a (SortList a) -> Bool       | Eq, Ord a      // is member of
insertSort    :: a (SortList a) -> SortList a | Ord a          // add element
removeFirst   :: a (SortList a) -> SortList a | Eq, Ord a      // remove first occurrance
removeAll     :: a (SortList a) -> SortList a | Eq, Ord a      // remove all occurrances
elements      ::   (SortList a) -> [a]                         // return all elements
count         ::   (SortList a) -> Int                         // number of elements

minimum       ::   (SortList a) -> a                           // current minimum value
maximum       ::   (SortList a) -> a                           // current maximum value

mergeSortList :: (SortList a) (SortList a) -> SortList a | Eq, Ord a // merge sorted lists
