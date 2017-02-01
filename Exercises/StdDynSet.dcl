definition module StdDynSet

import StdOverloaded

class Set a | TC, ==, toString a

:: Set

instance zero     Set
instance toString Set
instance ==       Set

toSet			:: a       -> Set  | Set a

nrOfElts		::     Set -> Int
isEmptySet		::     Set -> Bool

memberOfSet		:: a   Set -> Bool | Set a
isSubset		:: Set Set -> Bool
isStrictSubset	:: Set Set -> Bool

union			:: Set Set -> Set
intersection	:: Set Set -> Set
without			:: Set Set -> Set
