implementation module BinSearchTree

import StdEnv
import StdDebug
import BinTree

//  From the lecture nodes, p. 73:
insertTree							:: a (Tree a) -> Tree a | Ord a
insertTree e Leaf					= Node e Leaf Leaf
insertTree e (Node x le ri)
| e <= x							= Node x (insertTree e le) ri
| e >  x							= Node x le (insertTree e ri)

deleteTree							:: a (Tree a) -> Tree a | Eq, Ord a
deleteTree e Leaf					= Leaf
deleteTree e (Node x le ri)
| e <  x							= Node x (deleteTree e le) ri
| e == x							= join le ri
| e >  x							= Node x le (deleteTree e ri)
where
	join							:: (Tree a) (Tree a) -> Tree a
	join Leaf b2					= b2
	join b1   b2					= Node x b1` b2
	where
		(x,b1`)						= largest b1
		
		largest						:: (Tree a) -> (a,Tree a)
		largest (Node x b1 Leaf)	= (x,b1)
		largest (Node x b1 b2)		= (y,Node x b1 b2`)
		where
			(y,b2`)					= largest b2

//	The trees mentioned in the exercise booklet:
z0 :: Tree Int;		z0 = Leaf
z1 :: Tree Int;		z1 = insertTree 50 z0
z2 :: Tree Int;		z2 = insertTree 10 z1
z3 :: Tree Int;		z3 = insertTree 75 z2
z4 :: Tree Int;		z4 = insertTree 80 z3
z5 :: Tree Int;		z5 = insertTree 77 z4
z6 :: Tree Int;		z6 = insertTree 10 z5
z7 :: Tree Int;		z7 = insertTree 75 z6
z8 :: Tree Int;		z8 = deleteTree 50 z7

is_ordered						    :: (Tree a) -> Bool | Eq, Ord a
is_ordered _ = trace_n "is_ordered not yet implemented" False

is_balanced						    :: (Tree a) -> Bool
is_balanced _ = trace_n "is_balanced not yet implemented" False

