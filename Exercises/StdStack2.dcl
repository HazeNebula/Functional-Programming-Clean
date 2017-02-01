definition module StdStack2

::	Stack2 elem = E.impl:	{ stack		:: impl
							, push		:: elem impl -> impl
							, pop		::      impl -> impl
							, top		::      impl ->  elem
							, elements	::      impl -> [elem]
							}

push		:: elem (Stack2 elem) -> Stack2 elem
pop			::      (Stack2 elem) -> Stack2 elem
top			::      (Stack2 elem) ->  elem
elements	::      (Stack2 elem) -> [elem]
