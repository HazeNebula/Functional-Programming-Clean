implementation module StdStack2

import StdEnv
import StdDebug

::	Stack2 elem = E.impl:	{ stack		:: impl
							, push		:: elem impl -> impl
							, pop		::      impl -> impl
							, top		::      impl ->  elem
							, elements	::      impl -> [elem]
							}

//	What type error is given when you try to compile the following line? Explain what it means.
//test new stack2 = { stack2 & stack = stack2.push new stack2.stack }

push						:: elem (Stack2 elem) -> Stack2 elem
push _ stack = trace_n "push not yet implemented" stack

pop							:: (Stack2 elem) -> Stack2 elem
pop stack = trace_n "pop not yet implemented" stack

top							:: (Stack2 elem) -> elem
top _ = trace_n "top not yet implemented" (abort "top not yet implemented")

elements					:: (Stack2 elem) -> [elem]
elements _ = trace_n "elements not yet implemented" []

//	Compare StdStack and StdStack2. Are they the same? Explain your opinion.

