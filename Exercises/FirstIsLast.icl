module FirstIsLast

import StdEnv
import StdDebug

last1			:: [a] -> a
last1 [x]		= x
last1 [_:xs]	= last1 xs

last2			:: ([a] -> a)
last2			= hd o reverse

Start = ""

