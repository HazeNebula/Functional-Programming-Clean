module StdAVLTreeTest

/*	Test module StdAVLTree
	For working with Gast: 
		(*) use Environment 'Gast'
		(*) set Project Options to 'Basic Values Only'.
*/

import gast
import StdAVLTree

:: TList = T[Int]

derive bimap []

maxlen = 200

ggen{|TList|} n [r:rnd] = [T (removeDup (map (\x->x rem maxlen) a)) : ggen{|*|} n b]
where (a,b) = splitAt (((r rem maxlen)+maxlen) rem maxlen) rnd

genShow{|TList|} sep p (T ls) rest = genShow{|*|} sep p (take 10 ls) ["...":rest]

Start							= testn 1000
									(\n m -> let n` = abs ((n bitand 0x7FFFFFFF)>>24) in
										   right_increasing_remains_AVL n`
										/\ left_increasing_remains_AVL  n`
										/\ random_is_AVL                   m
										/\ delete_remains_AVL              m
										/\ member_after_insert          n` m
										/\ no_member_after_delete          m
										/\ member_after_insert2         n` m
										/\ member_after_delete_other       m
										/\ True
									)

rightAVL						:: Int -> AVLTree Int
rightAVL n						= foldr insertAVLTree mkAVLLeaf [1..n]

leftAVL							:: Int -> AVLTree Int
leftAVL n						= foldr insertAVLTree mkAVLLeaf [n,n-1..1]

// does not work as expected
complexity_burner				:: Bool
complexity_burner				= force empty_avl
where
   empty_avl					= foldr deleteAVLTree (rightAVL many) [many,many-1..1]
   
   force :: !(AVLTree a) -> Bool
   force _ = True
   
   many = 200000

right_increasing_remains_AVL	:: Int -> Property
right_increasing_remains_AVL n	= name "right_increasing_remains_AVL"
								  (isAVLTree (rightAVL n))

left_increasing_remains_AVL		:: Int -> Property
left_increasing_remains_AVL n	= name "left_increasing_remains_AVL"
								  (isAVLTree (leftAVL n))

random_is_AVL					:: TList -> Property
random_is_AVL (T ls)			= name "random_is_AVL" 
								  (isAVLTree (avl))
where
	avl							= foldr insertAVLTree mkAVLLeaf ls

delete_remains_AVL				:: TList -> Property
delete_remains_AVL (T ls)		= name "delete_remains_AVL" 
								  (ForEach ls 
								   \x -> isAVLTree (deleteAVLTree x avl)
								  )
where
	avl							= foldr insertAVLTree mkAVLLeaf ls

member_after_insert				:: Int TList -> Property
member_after_insert n (T ls)	= name "member_after_insert"
								  (not (isMemberAVLTree n avl)
								     ==>
								   isMemberAVLTree n (insertAVLTree n avl)
								  )
where
	avl							= foldr insertAVLTree mkAVLLeaf ls

no_member_after_delete			:: TList -> Property
no_member_after_delete (T ls)	= name "no_member_after_delete"
								  (ForEach ls
									\x->not (isMemberAVLTree x (deleteAVLTree x avl))
								  )
where
	avl							= foldr insertAVLTree mkAVLLeaf ls

member_after_insert2			:: Int TList -> Property
member_after_insert2 n (T ls)	= name "member_after_insert2"
								  (ForEach ls
								    \x->isMemberAVLTree x (insertAVLTree n avl)
								  )
where
	avl							= foldr insertAVLTree mkAVLLeaf ls

member_after_delete_other		:: TList -> Property
member_after_delete_other (T ls)= name "member_after_delete_other"
								  (ForEach ls
								    \x->(ForEach ls 
									      \y->x<>y==>isMemberAVLTree y (deleteAVLTree x avl)
								         )
								  )
where
	avl							= foldr insertAVLTree mkAVLLeaf ls
