implementation module BinTreeTraversal

import StdEnv
import StdDebug
import BinTree

testTree = Node 10 
			(Node 6 
				(Node 2 Leaf (Node 4 Leaf (Node 5 Leaf Leaf)))
				(Node 7 Leaf Leaf)
			) 
			(Node 14 
				(Node 12
					(Node 11 Leaf Leaf)
					(Node 13 Leaf Leaf)
				)
				(Node 17 Leaf Leaf)
			)

listAscending							:: (Tree a) -> [a]
listAscending _ = trace_n "listAscending not yet implemented" []

listDescending							:: (Tree a) -> [a]
listDescending _ = trace_n "listDescending not yet implemented" []

listToLeaves							:: (Tree a) -> [a]
listToLeaves _ = trace_n "listToLeaves not yet implemented" []

Start = (listAscending  testTree, '\n'
        ,listDescending testTree, '\n'
        ,listToLeaves   testTree
        )
