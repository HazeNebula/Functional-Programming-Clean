module StdDynSetTest

/*	Test module StdDynSet
	for using Gast:
		(*) set environment 'Everything' (because of dynamics)
		(*) set Project Options to 'Basic Values Only' and '16M' Maximum Heap Size.
*/

import gast
import StdDynSet

Start = testn 2000
		(\ints reals strings -> let set = Union [listToSet ints,listToSet reals,listToSet strings]
		                         in    empty_set_has_zero_elements
		                            /\ add_identical_elements_is_identity set ints reals strings
		                            /\ added_elements_are_member_of_set   set ints reals strings
		                            /\ set_is_subset_of_itself            set
		                            /\ elems_not_subset_of_set_wo_elems   set ints
		                            /\ set_is_not_strict_subset_of_itself set
		                            /\ union_of_set_is_identity           set
		                            /\ union_with_zero_is_identity        set
		                            /\ intersection_of_set_is_identity    set
		                            /\ intersection_with_zero_is_zero     set
		                            /\ set_without_itself_is_zero         set
		                            /\ removed_elems_not_member_of_set    set ints
		                            /\ without_zero_is_identity           set
		                            /\ True
		)

empty_set_has_zero_elements :: Property
empty_set_has_zero_elements
	= name "empty_set_has_zero_elements"
		(nrOfElts emptyset == 0)
where
	emptyset :: Set
	emptyset = zero

add_identical_elements_is_identity :: Set [Int] [Real] [String] -> Property
add_identical_elements_is_identity set ints=:[int:_] reals=:[real:_] strings=:[string:_]
	= name "add_identical_elements_is_identity"
		(and [add int set == set, add real set == set, add string set == set])
add_identical_elements_is_identity _ _ _ _
	= name "add_identical_elements_is_identity" True

added_elements_are_member_of_set :: Set [Int] [Real] [String] -> Property
added_elements_are_member_of_set set ints reals strings
	= name "added_elements_are_member_of_set"
		(and (flatten [map (flip memberOfSet set) ints, map (flip memberOfSet set) reals, map (flip memberOfSet set) strings]))

set_is_subset_of_itself :: Set -> Property
set_is_subset_of_itself set
	= name "set_is_subset_of_itself" (isSubset set set)

elems_not_subset_of_set_wo_elems :: Set [Int] -> Property
elems_not_subset_of_set_wo_elems set ints
	= name "elems_not_subset_of_set_wo_elems" (isEmpty ints || not (isSubset intsset (without set intsset)))
where
	intsset = listToSet ints

set_is_not_strict_subset_of_itself :: Set -> Property
set_is_not_strict_subset_of_itself set
	= name "set_is_not_strict_subset_of_itself" (not (isStrictSubset set set))

union_of_set_is_identity :: Set -> Property
union_of_set_is_identity set
	= name "union_of_set_is_identity" (union set set == set)

intersection_of_set_is_identity :: Set -> Property
intersection_of_set_is_identity set
	= name "intersection_of_set_is_identity" (intersection set set == set)

set_without_itself_is_zero :: Set -> Property
set_without_itself_is_zero set
	= name "set_without_itself_is_zero" (without set set == zero)

removed_elems_not_member_of_set :: Set [Int] -> Property
removed_elems_not_member_of_set set ints
	= name "removed_elems_not_member_of_set" (not (any (flip memberOfSet (without set (listToSet ints))) ints))

union_with_zero_is_identity :: Set -> Property
union_with_zero_is_identity set
	= name "union_with_zero_is_identity"
		(union set zero == set && union zero set == set && set == union set zero && set == union zero set)

intersection_with_zero_is_zero :: Set -> Property
intersection_with_zero_is_zero set
	= name "intersection_with_zero_is_zero"
		(intersection set zero == zero && intersection zero set == zero && zero == intersection set zero && zero == intersection zero set)

without_zero_is_identity :: Set -> Property
without_zero_is_identity set
	= name "without_zero_is_identity" (without set zero == set && without zero set == zero)

listToSet :: [a] -> Set | Set a
listToSet xs = Union (map toSet xs)

Union :: [Set] -> Set
Union sets = foldr union zero sets

add :: a Set -> Set | Set a
add x set = union (toSet x) set
