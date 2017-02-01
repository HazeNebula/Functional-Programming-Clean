implementation module StdSet

import StdBool, StdClass, StdFunc, StdList, StdMisc
import StdDebug

::	Set a  // define a suitable implementation for sets

toSet :: [a] -> Set a | Eq a
toSet _ = trace_n "toSet not yet implemented" (abort "Set not yet implemented")

fromSet :: (Set a) -> [a]
fromSet _ = trace_n "fromSet not yet implemented" []

memberOfSet :: a (Set a) -> Bool | Eq a
memberOfSet _ _ = trace_n "memberOfSet not yet implemented" False

instance zero (Set a) where zero = trace_n "instance zero (Set a) not yet implemented" (abort "Set not yet implemented")

isSubset :: (Set a) (Set a) -> Bool | Eq a
isSubset _ _ = trace_n "isSubset not yet implemented" False

isStrictSubset :: (Set a) (Set a) -> Bool | Eq a
isStrictSubset _ _ = trace_n "isStrictSubset not yet implemented" False

instance == (Set a) | Eq a where == _ _ = trace_n "instance == (Set a) not yet implemented" False

union :: (Set a) (Set a) -> Set a | Eq a
union _ _ = trace_n "union not yet implemented" (abort "Set not yet implemented")

intersection :: (Set a) (Set a) -> Set a | Eq a
intersection _ _ = trace_n "intersection not yet implemented" (abort "Set not yet implemented")

without :: (Set a) (Set a) -> Set a | Eq a
without _ _ = trace_n "without not yet implemented" (abort "Set not yet implemented")

powerSet :: (Set a) -> Set (Set a)
powerSet _ = trace_n "powerSet not yet implemented" (abort "Set not yet implemented")

isEmptySet :: (Set a) -> Bool
isEmptySet _ = trace_n "isEmptySet not yet implemented" False

isDisjoint :: (Set a) (Set a) -> Bool | Eq a
isDisjoint _ _ = trace_n "isDisjoint not yet implemented" False

product :: (Set a) (Set b) -> Set (a,b)
product _ _ = trace_n "product not yet implemented" (abort "Set not yet implemented")

numberOfElements :: (Set a) -> Int
numberOfElements _ = trace_n "numberOfElements not yet implemented" zero

