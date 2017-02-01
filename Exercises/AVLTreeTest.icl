module AVLTreeTest

import StdEnv
import StdDebug
import StdAVLTree, Subs

//	Test all ways to delete elements:
Start			= filter (not o isAVLTree) (map (foldr deleteAVLTree tree) (subs elements))
where
	elements	= [3,2,1,0,1,2,3,2,1,0,1,2,3]
	tree		= foldr insertAVLTree mkAVLLeaf elements
