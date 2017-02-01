implementation module GenTreePrint

import StdEnv
import StdDebug
import GenTree
//import TextCompose	// uncomment this line only if you have implemented TextCompose
import StringUtil

instance toString (GenTree a b) | toString a & toString b where
	toString tree					= indentTree tree
//	toString tree					= tree2D tree	// uncomment this line only if you're going for the tree2D function

indentTree							:: !(GenTree a b) -> String | toString a & toString b
indentTree _ = trace_n "indentTree not yet implemented" ""

