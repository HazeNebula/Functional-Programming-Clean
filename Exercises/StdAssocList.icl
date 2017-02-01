implementation module StdAssocList

import StdClass
import StdList
import StdDebug

::  AssocList k a :== [(k,a)]

newAssocList		:: AssocList k a
newAssocList = trace_n "newAssocList not yet implemented" []

countValues			:: (AssocList k a) -> Int
countValues _ = trace_n "countValues not yet implemented" zero

lookupKey			:: k (AssocList k a) -> [a] | Eq, Ord k
lookupKey _ _ = trace_n "lookupKey not yet implemented" []

updateKey			:: k a (AssocList k a) -> AssocList k a | Eq, Ord k
updateKey _ _ _ = trace_n "updateKey not yet implemented" []

removeKey			:: k (AssocList k a) -> AssocList k a | Eq, Ord k
removeKey _ _ = trace_n "removeKey not yet implemented" []

Start = ""
