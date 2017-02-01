module FragsTest

/*	Test module Frags
	For working with Gast: 
		(*) Use Environment 'Gast'
		(*) Set Project Options to 'Basic Values Only'
*/

import StdEnv
import gast
import Frags

Start						= testn 1000
								( \xs ->    length_is_correct     xs
								         /\ all_lengths_occur     xs
								         /\ all_elements_are_frag xs
								         /\ True
								)

length_is_correct			:: [Int] -> Property
length_is_correct xs		= name "length_is_correct"
							       (let n = length xs
							         in length (frags xs) == 1 + n*(n+1)/2
							       )

all_lengths_occur		:: [Int] -> Property
all_lengths_occur xs	= name "all_lengths_occur"
							       (let n = length xs
							         in sort (removeDup (map length (frags xs))) == [0..n]
							       )

all_elements_are_frag	:: [Int] -> Property
all_elements_are_frag xs	= name "alle_elementen_zijn_frag"
							       (and [is_frag f xs \\ f <- frags xs])

/*	is_frag fragment list is True iff fragment is a strict sublist of list;
 *	i.e. all elements of fragment directly follow each other in list.
 */
is_frag						:: [a] [a] -> Bool | Eq a
is_frag [] _				= True
is_frag _  []				= False
is_frag [f:fs] [x:xs]
| f == x
	| is_frag fs xs			= True
	| otherwise				= is_frag [f:fs] xs
| otherwise					= is_frag [f:fs] xs
