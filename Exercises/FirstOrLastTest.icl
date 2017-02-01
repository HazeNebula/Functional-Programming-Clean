module FirstOrLastTest

/*	Test module FistOrLast
	To werken with Gast:
		(*) use Environment 'Gast'
		(*) set Project Options to 'Basic Values Only'
*/

import FirstOrLast
import StdEnv
import gast

Start	= testn 1000
			(\n v ->
			      first2_is_first_2   v
			   /\ last2_is_last_2     v
			   /\ first_is_first_n  n v
			   /\ lastn_is_last_n   n v
			   /\ True
			)

:: BaseType :== Int

first2_is_first_2			:: [BaseType] -> Property
first2_is_first_2 l			= name "first2 is first two"
									((length l > 1) ==> let l` = first2 l in same_part 2 l` l)

last2_is_last_2				:: [BaseType] -> Property
last2_is_last_2 l			= name "last2 is last twee"
									((length l > 1) ==> let l` = last2 l in same_part 2 l` (drop (length l - 2) l))

first_is_first_n			:: Int [BaseType] -> Property
first_is_first_n n l		= name "firstn is first n"
									((n >= 0 && n < length l) ==> let l` = firstn n l in same_part n l` l)

lastn_is_last_n				:: Int [BaseType] -> Property
lastn_is_last_n n l			= name "lastn is last n"
									((n >= 0 && n < length l) ==> let l` = lastn n l in same_part n l` (drop (length l - n) l))

same_part					:: Int [BaseType] [BaseType] -> Bool
same_part 0 _ _				= True
same_part n [a:as] [b:bs]	= a == b && same_part (n-1) as bs
same_part _ _ _				= False
