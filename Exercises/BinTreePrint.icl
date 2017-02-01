implementation module BinTreePrint

import StdEnv
import StdDebug
import BinTree 

Start								= map (flip (+++) "\n" o toString) [t0,t1,t2,t3,t4,t5,t6,t7]

instance toString (Tree a) | toString a where
	toString tree					= indentTree tree
//	toString tree					= tree2D tree

indentTree							:: !(Tree a) -> String | toString a
indentTree _ = trace_n "indentTree not yet implemented" ""

tree2D								:: !(Tree a) -> String | toString a
tree2D _ = trace_n "trace2D not yet implemented" ""

