definition module StdGameTree

import StdRoseTree

::	Moves   s	:==	s -> [s]
::	Worth   s w	:==	s -> w

gametree		:: (Moves s) s -> RoseTree s
minimaxvalue	::       (RoseTree w) -> w          | Ord,~    w
ab_minimaxvalue	:: (w,w) (RoseTree w) -> w          | Ord,~,Eq w
minimaxtree		::       (RoseTree w) -> RoseTree w | Ord,~    w

nextmoves		:: PruneDepth (Worth s w) (Moves s) s -> [s] | Ord,~,Eq w
