implementation module StdGenTree

/**	This module defines generalised trees.
*/
import StdEnv

root					:: !(GenTree a) -> a
root (Node r _)			= r

children				:: !(GenTree a) -> [GenTree a]
children (Node _ ts)	= ts

depth					:: !(GenTree a) -> Int
depth (Node _ [])		= 1
depth (Node _ xs)		= 1 + maxList (map depth xs)

iteratetree				:: !(Children a) a -> GenTree a
iteratetree f s			= Node s (map (iteratetree f) (f s))

prunetree				:: !PruneDepth !(GenTree a) -> GenTree a
prunetree 1 (Node x ts)	= Node x []
prunetree n (Node x ts)	= Node x (map (prunetree (n-1)) ts)

maptree					:: (a -> b) !(GenTree a) -> GenTree b
maptree f (Node x ts)	= Node (f x) (map (maptree f) ts)
