definition module SortedFileToTree

import StdFile

::	BTree a		= Leaf
				| Node (BTree a) a (BTree a)

readSortedFile	:: String *env -> (BTree String,*env) | FileSystem env
writeSortedFile	:: String (BTree String) *env -> *env | FileSystem env
