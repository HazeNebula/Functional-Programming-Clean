module SubsTest

/*	Test module Subs
	Voor working with Gast: 
		(*) Use Environment 'Gast'
		(*) Set Project Options to 'Basic Values Only'
*/

import StdEnv
import gast
import Subs

Start
							= testn 1000
							    (\xs ->    all_lengths_occur     xs
							            /\ all_elements_are_sub  xs
							            /\ subs_xs_contains_xs   xs
							            /\ True
							    )

all_lengths_occur			:: [Int] -> Property
all_lengths_occur xs		= name "All lengths occur"
							       (let n = length xs
							         in sort (removeDup (map length (subs xs))) == [0..n]
							       )

all_elements_are_sub		:: [Int] -> Property
all_elements_are_sub xs		= name "all_elements_are_sub" (and [is_sub f xs \\ f <- subs xs])

subs_xs_contains_xs			:: [Int] -> Property
subs_xs_contains_xs xs		= name "subs_xs_contains_xs" (isMember xs (subs xs))

/*	is_sub sub list is True only if all elements of sub consequtively occur in list.
*/
is_sub						:: [a] [a] -> Bool | Eq a
is_sub [] _					= True
is_sub _ []					= False
is_sub [x:xs] [y:ys]
| x == y					= is_sub xs ys
| otherwise					= is_sub [x:xs] ys
