implementation module StdGameTree

import StdRoseTree
import StdEnv

//	compute game tree using moves function and begin state:
gametree							:: (Moves s) s -> RoseTree s
gametree moves s					= iteratetree moves s

//	compute minimax-value of root of game tree:
minimaxvalue						:: (RoseTree w) -> w | Ord,~ w
minimaxvalue (Node w [])			= w
minimaxvalue (Node _ ts)			= w
where	ws							= map minimaxvalue ts
		w							= ~(minList ws)

//	compute minimax-value with alpha-beta pruning:
ab_minimaxvalue						:: (w,w) (RoseTree w) -> w | Ord,~,Eq w
ab_minimaxvalue (a,b) (Node w [ ])	= max a (min w b)
ab_minimaxvalue (a,b) (Node _ ts)	= bound (a,b) ts 
where
	bound							:: (w,w) [RoseTree w] -> w | Ord,~,Eq w
	bound (a,b) []					= a
	bound (a,b) [t : ts]			= if (a` == b) a` (bound (a`,b) ts)
	where a`						= ~ (ab_minimaxvalue (~b,~a) t)

//	compute minimax-values of all nodes of the game tree:
minimaxtree							:: (RoseTree w) -> RoseTree w | Ord,~ w
minimaxtree (Node w [])				= Node w []
minimaxtree (Node _ ts)				= Node w ts`
where	ts`							= map minimaxtree ts
		w							= ~(minList (map root ts`))

//	compute look-ahead of n rounds, and compute all minimax-values of the nodes:
minimax								:: PruneDepth (Worth s w) (Moves s) -> s -> RoseTree w | Ord, ~ w
minimax n w moves					= minimaxtree 
									o (maptree w) 
									o (prunetree (2*n)) 
									o (gametree moves)

//	select optimal next move, with look-ahead n rounds, using the minimax-values of all nodes
//  of the game tree:
nextmoves							:: PruneDepth (Worth s w) (Moves s) s -> [s] | Ord,~,Eq w
nextmoves n w moves s				= [ s` \\ s` <- moves s
									        & w` <- children mtree
									        | ~(root w`) == root mtree
									  ]
where mtree							= minimax n w moves s

//	sorting sub trees for better states first:
high (Node w ts)					= Node w (sortBy higher (map low  ts))
low  (Node w ts)					= Node w (sortBy lower  (map high ts))
higher t1 t2						= root t1 >  root t2
lower  t1 t2						= root t1 <= root t2
