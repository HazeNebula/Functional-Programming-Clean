definition module StdRoseTree

import StdClass

/**	This module defines rose trees.
*/
::	RoseTree a	=	Node a [RoseTree a]
::	Children a	:== a -> [a]
::	PruneDepth	:== Int

iteratetree		:: !(Children a) a ->  RoseTree a

root			:: !(RoseTree a)   -> a
children		:: !(RoseTree a)   -> [RoseTree a]
depth			:: !(RoseTree a)   -> Int

maptree			:: (a -> b)    !(RoseTree a) -> RoseTree b
prunetree		:: !PruneDepth !(RoseTree a) -> RoseTree a
bonsai			::             !(RoseTree a) -> RoseTree a | Eq a
paths			::             !(RoseTree a) -> [[a]]
