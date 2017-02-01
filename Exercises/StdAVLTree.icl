implementation module StdAVLTree

import StdEnv
import StdDebug

::	AVLTree  a // give a definition of AVLTree here

//	Access functions for AVL trees:
mkAVLLeaf							:: AVLTree a
mkAVLLeaf = abort "mkAVLLeaf not yet implemented"

mkAVLNode							:: a -> AVLTree a
mkAVLNode _ = abort "mkAVLNode not yet implemented"

//	isMemberAVLTree x tree yields True iff x is a value in tree.
isMemberAVLTree						:: a (AVLTree a) -> Bool | Eq, Ord a
isMemberAVLTree _ _ = trace_n "isMemberAVLTree not yet implemented" False

//	insertAVLTree x tree inserts x in tree in such a way that tree remains an AVL tree.
insertAVLTree						:: a (AVLTree a) -> AVLTree a | Eq, Ord a
insertAVLTree _ tree = trace_n "insertAVLTree not yet implemented" tree

//	deleteAVLTree x tree removes entry x from tree.
deleteAVLTree						:: a (AVLTree a) -> AVLTree a	| Eq, Ord a
deleteAVLTree _ tree = trace_n "deleteAVLTree not yet implemented" tree

isAVLTree							:: (AVLTree a) -> Bool | Eq, Ord a
isAVLTree _ = trace_n "isAVLTree not yet implemented" False

