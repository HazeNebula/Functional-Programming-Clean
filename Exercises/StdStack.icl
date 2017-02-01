implementation module StdStack

import StdEnv
import StdDebug

:: Stack a // define a suitable implementation for stacks

newStack			:: Stack a
newStack = trace_n "newStack not yet implemented" (abort "Stack not yet implemented")

push				:: a (Stack a) -> Stack a
push _ _ = trace_n "push not yet implemented" (abort "Stack not yet implemented")

pushes				:: [a] (Stack a) -> Stack a
pushes _ _ = trace_n "pushes not yet implemented" (abort "Stack not yet implemented")

pop					:: (Stack a) -> Stack a
pop _ = trace_n "pop not yet implemented" (abort "Stack not yet implemented")

popn				:: Int (Stack a) -> Stack a
popn _ _ = trace_n "popn not yet implemented" (abort "Stack not yet implemented")

top					:: (Stack a) -> a
top _ = trace_n "top not yet implemented" (abort "Stack not yet implemented")

topn				:: Int (Stack a) -> [a]
topn _ _ = trace_n "topn not yet implemented" []

elements			:: (Stack a) -> [a]
elements _ = trace_n "elements not yet implemented" []

count				:: (Stack a) -> Int
count _ = trace_n "count not yet implemented" zero

//	You can use this Start-function to test your implementations:
Start				= ( "s0 = newStack = ",        s0,'\n'
					  , "s1 = push 1 s0 = ",       s1,'\n'
					  , "s2 = pushes [2..5] s1 = ",s2,'\n'
					  , "s3 = pop s2 = ",          s3,'\n'
					  , "s4 = popn 3 s3 = ",       s4,'\n'
					  , "s5 = top s4 = ",          s5,'\n'
					  , "s6 = topn 3 s2 = ",       s6,'\n'
					  , "s7 = elements s2 = ",     s7,'\n'
					  )
where
	s0				= newStack
	s1				= push   1      s0
	s2				= pushes [2..5] s1
	s3				= pop           s2
	s4				= popn     3    s3
	s5				= top           s4
	s6				= topn     3    s2
	s7				= elements      s2
