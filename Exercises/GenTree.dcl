definition module GenTree

import StdClass

::  GenTree a b  = Leaf b | Node a [GenTree a b]

::  Either  a b  = This a | That b

root         ::   (GenTree a b) -> Either a b
trees        ::   (GenTree a b) -> [GenTree a b]

isNodeMember :: a (GenTree a b) -> Bool | Eq a
isLeafMember :: b (GenTree a b) -> Bool | Eq b
allNodes     ::   (GenTree a b) -> [a]
allLeaves    ::   (GenTree a b) -> [b]
allMembers   ::   (GenTree a a) -> [a]

map2         :: (a -> c,b -> d) (GenTree a b) -> GenTree c d
