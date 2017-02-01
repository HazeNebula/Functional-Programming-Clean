definition module StdSet

import StdClass

::	Set a

toSet			:: [a]             -> Set a | Eq a
fromSet			:: (Set a)         -> [a]

isEmptySet		:: (Set a)         -> Bool
isDisjoint		:: (Set a) (Set a) -> Bool  | Eq a
isSubset		:: (Set a) (Set a) -> Bool  | Eq a
isStrictSubset	:: (Set a) (Set a) -> Bool  | Eq a
memberOfSet		:: a       (Set a) -> Bool  | Eq a
union           :: (Set a) (Set a) -> Set a | Eq a
intersection	:: (Set a) (Set a) -> Set a | Eq a
numberOfElements:: (Set a) -> Int
without			:: (Set a) (Set a) -> Set a | Eq a

product			:: (Set a) (Set b) -> Set (a,b)

instance zero (Set a)
instance ==   (Set a) | Eq a

powerSet		:: (Set a)         -> Set (Set a)
