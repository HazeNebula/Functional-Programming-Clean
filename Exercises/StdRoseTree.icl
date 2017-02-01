implementation module StdRoseTree

/**	This module defines rose trees.
*/
import StdEnv

root							:: !(RoseTree a) -> a
root (Node r _)					= r

children						:: !(RoseTree a) -> [RoseTree a]
children (Node _ ts)			= ts

depth							:: !(RoseTree a) -> Int
depth (Node _ [])				= 1
depth (Node _ xs)				= 1 + maxList (map depth xs)

prunetree						:: !PruneDepth !(RoseTree a) -> RoseTree a
prunetree d (Node x ts)
| d <= 1						= Node x []
| otherwise						= Node x (map (prunetree (d-1)) ts)

bonsai							:: !(RoseTree a) -> RoseTree a | Eq a
bonsai t						= bonsai` [] t
where
	bonsai`						:: ![a] !(RoseTree a) -> RoseTree a | Eq a
	bonsai` path (Node v ts)	= Node v (filter (\t -> not (isMember (root t) [v:path]))
								                 (map (bonsai` [v:path]) ts)
								         )

iteratetree						:: !(Children a) a -> RoseTree a
iteratetree f s					= Node s (map (iteratetree f) (f s))

maptree							:: (a -> b) !(RoseTree a) -> RoseTree b
maptree f (Node x ts)			= Node (f x) (map (maptree f) ts)

paths							:: !(RoseTree a) -> [[a]]
paths (Node x [])				= [[x]]
paths (Node x ts)				= [[x:path] \\ t <- ts, path <- paths t]
