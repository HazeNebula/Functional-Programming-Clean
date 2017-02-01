definition module StdGenTree

import StdClass

/**	This module defines generalised trees.
*/
::	GenTree  a	=	Node a [GenTree a]
::	Children a	:== a -> [a]
::	PruneDepth	:== Int

root			:: !(GenTree  a)   -> a
children		:: !(GenTree  a)   -> [GenTree a]
iteratetree		:: !(Children a) a ->  GenTree a
depth			:: !(GenTree  a)   -> Int
prunetree		:: !PruneDepth !(GenTree a) -> GenTree a
maptree			:: (a -> b)    !(GenTree a) -> GenTree b
