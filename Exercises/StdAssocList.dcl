definition module StdAssocList

import StdClass

::  AssocList k a

newAssocList ::      AssocList k a                               // empty association list
countValues  ::     (AssocList k a) -> Int                       // number of elements
lookupKey    :: k   (AssocList k a) -> [a]           | Eq, Ord k // element with key value
updateKey    :: k a (AssocList k a) -> AssocList k a | Eq, Ord k // change value associated with key
removeKey    :: k   (AssocList k a) -> AssocList k a | Eq, Ord k // remove element
