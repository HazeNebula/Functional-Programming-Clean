implementation module GenTree

import StdEnv
import StdDebug

::  GenTree a b					= Leaf b | Node a [GenTree a b]
::  Either  a b					= This a | That b

root							:: (GenTree a b) -> Either a b
root _ = trace_n "root not yet implemented" (abort "root not yet implemented")

trees							:: (GenTree a b) -> [GenTree a b]
trees _ = trace_n "trees not yet implemented" []

isNodeMember					:: a (GenTree a b) -> Bool | Eq a
isNodeMember _ _ = trace_n "isNodeMember not yet implemented" False

isLeafMember					:: b (GenTree a b) -> Bool | Eq b
isLeafMember _ _ = trace_n "isLeafMember not yet implemented" False

allNodes						:: (GenTree a b) -> [a]
allNodes _ = trace_n "allNodes not yet implemented" []

allLeaves						:: (GenTree a b) -> [b]
allLeaves _ = trace_n "allLeaves not yet implemented" []

allMembers						:: (GenTree a a) -> [a]
allMembers _ = trace_n "allMembers not yet implemented" []

map2							:: (a -> c,b -> d) (GenTree a b) -> GenTree c d
map2 _ _ = trace_n "map2 not yet implemented" (abort "map2 not yet implemented")

Start							= root (Node 5 [Node 3 [Node 6 [Leaf 42.42]],Leaf 3.14])
