definition module StdAVLTree

import StdClass

::	AVLTree a

mkAVLLeaf		::                  AVLTree a
mkAVLNode		:: a             -> AVLTree a
isMemberAVLTree	:: a (AVLTree a) -> Bool		| Eq, Ord a
insertAVLTree	:: a (AVLTree a) -> AVLTree a	| Eq, Ord a
deleteAVLTree	:: a (AVLTree a) -> AVLTree a	| Eq, Ord a

isAVLTree		::   (AVLTree a) -> Bool		| Eq, Ord a
