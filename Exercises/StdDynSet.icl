implementation module StdDynSet

import StdEnv
import StdDebug
import StdBool
import StdDynamic

class Set a | TC, ==, toString a

:: Set // define your implementation of Set here

instance zero     Set     where zero = trace_n "instance zero Set not yet implemented" (abort "Set not yet implemented")

instance toString Set     where toString _ = trace_n "instance toString Set not yet implemented" ""

instance toString Dynamic where toString _ = trace_n "instance toString Dynamic not yet implemented" ""

instance ==       Set     where == _ _ = trace_n "instance == Set not yet implemented" False

memberOfSet					:: a Set -> Bool | Set a
memberOfSet _ _ = trace_n "memberOfSet not yet implemented" False

nrOfElts					:: Set -> Int
nrOfElts _ = trace_n "nrOfElts not yet implemented" zero

isEmptySet					:: Set -> Bool
isEmptySet _ = trace_n "isEmptySet not yet implemented" False

toSet						:: a -> Set | Set a
toSet _ = trace_n "toSet not yet implemented" (abort "Set not yet implemented")

union						:: Set Set -> Set
union _ _ = trace_n "union not yet implemented" (abort "Set not yet implemented")

intersection				:: Set Set -> Set
intersection _ _ = trace_n "intersection not yet implemented" (abort "Set not yet implemented")

isSubset					:: Set Set -> Bool
isSubset _ _ = trace_n "isSubset not yet implemented" False

isStrictSubset				:: Set Set -> Bool
isStrictSubset _ _ = trace_n "isStrictSubset not yet implemented" False

without						:: Set Set -> Set
without _ _ = trace_n "without not yet implemented" (abort "Set not yet implemented")

//	You can use this Start-function to test your implementation:
Start = (toString (remove 42 set), toString (toSet set))
where
	set = foldl union zero [toSet 42,toSet False,toSet "Hello World!",toSet -123.456]
