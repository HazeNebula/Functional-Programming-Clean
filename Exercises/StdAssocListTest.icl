module StdAssocListTest

/*	Test module StdAssocList
	For working with Gast: 
		(*) gebruik Environment 'Gast'
		(*) Set Project Options to 'Basic Values Only' and '2M' Maximum Heap Size
*/

import gast
import StdAssocList

Start							= testn 1000
									(\x ->
									      newAssocList_is_empty
									   /\ number_of_elements_correct x
									   /\ lookup_after_update        x
									   /\ lookup_after_update2       x
									   /\ keys_are_unique            x
									   /\ True
									)

newIntStringAssocList			:: AssocList Int String
newIntStringAssocList			= newAssocList

fillIntStringAssocList			:: (AssocList Int String) Int -> AssocList Int String
fillIntStringAssocList l n		= seq [updateKey k (toString k) \\ k <- [1..n]] l

newAssocList_is_empty			:: Property
newAssocList_is_empty			= name "newAssocList_is_empty"
								  (countValues newIntStringAssocList == 0)

number_of_elements_correct		:: Int -> Property
number_of_elements_correct n	= name "number_of_elements_correct"
								  (countValues (fillIntStringAssocList newIntStringAssocList n`) == max 0 n`)
where                  n`		= min n 100

lookup_after_update				:: Int -> Property
lookup_after_update n			= name "lookup_after_update"
								  (lookupKey k (updateKey k v (fillIntStringAssocList newIntStringAssocList n`)) == [v])
where                     n`	= min n 100
                          k		= n+1
                          v		= toString k

lookup_after_update2			:: Int -> Property
lookup_after_update2 n			= name "lookup_after_update2"
								  (lookupKey k (fillIntStringAssocList (updateKey k v newIntStringAssocList) n`) == [v])
where                     n`	= min n 100
                          k		= n+1
                          v		= toString k

keys_are_unique					:: Int -> Property
keys_are_unique n				= name "keys_are_unique"
								  (lookupKey k (removeKey k (fillIntStringAssocList newIntStringAssocList n`)) == [])
where                     n`	= min n 100
                          k		= n/2
