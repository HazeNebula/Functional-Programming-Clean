module SeqAndSeqList

import StdEnv
import StdDebug
import StdStack

//	You can use this Start-rule to test your implementations of pushes`, popn`, topn`, elements`.
//	The result should be True.
Start         = and
                [ elements s1           == elements s2
                , elements (popn` 3 s1) == elements (popn 3 s2)
                ,           topn` 3 s1  ==           topn 3 s2
                ]
where (s1,s2) = (pushes` [1..5] newStack, pushes [1..5] newStack)

pushes` = trace_n "pushes` not yet implemented" pushes

popn` = trace_n "popn` not yet implemented" popn

topn` = trace_n "topn` not yet implemented" topn

