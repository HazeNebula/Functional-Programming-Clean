module StdStringnumTest

/*	Test module StdStringnum
	To work with Gast: 
		(*) use Environment 'Gast'
		(*) set Project Options to 'Basic Values Only'
*/

import StdStringnum
import StdEnv
import gast

Start	= testn 1000
			(\a b -> 
		          zero_is_neutral_for_addition        b
		       /\ subtraction_of_self_is_zero         b
		       /\ subtraction_of_larger_yields_zero a b
		       /\ subtraction_of_less_is_identity   a b
		       /\ one_is_neutral_for_multiplication   b
		       /\ one_is_neutral_for_division         b
		       /\ zero_is_zero_for_multiplication     b
		       /\ divide_by_self_is_identity          b
		       /\ though_shant_overflow             a b
		       /\ True
		    )

zero_is_neutral_for_addition			:: Stringnum -> Property
zero_is_neutral_for_addition a			= name "zero_is_neutral_for_addition"
										       (zero + a == a && a == a + zero)

subtraction_of_self_is_zero				:: Stringnum -> Property
subtraction_of_self_is_zero a			= name "subtraction_of_self_is_zero"
										       (a - a == zero)

subtraction_of_larger_yields_zero		:: Stringnum Stringnum -> Property
subtraction_of_larger_yields_zero a b	= name "subtraction_of_larger_yields_zero"
										       (a > b ==> b - a == zero)

subtraction_of_less_is_identity			:: Stringnum Stringnum -> Property
subtraction_of_less_is_identity a b		= name "subtraction_of_less_is_identity"
										       (a < b ==> (b - a) + a == b /\ a + (b - a) == b)

one_is_neutral_for_multiplication		:: Stringnum -> Property
one_is_neutral_for_multiplication a		= name "one_is_neutral_for_multiplication" 
										       (one * a == a && a == a * one)

one_is_neutral_for_division				:: Stringnum -> Property
one_is_neutral_for_division a			= name "one_is_neutral_for_division"
									       (a / one == a)

zero_is_zero_for_multiplication			:: Stringnum -> Property
zero_is_zero_for_multiplication a		= name "zero_is_zero_for_multiplication"
										       (zero * a == zero && zero == a * zero)

divide_by_self_is_identity				:: Stringnum -> Property
divide_by_self_is_identity a			= name "divide_by_self_is_identity"
										       (a <> zero ==> a / a == one)

though_shant_overflow					:: Stringnum Stringnum -> Property
though_shant_overflow n a				= name "though_shant_overflow"
										       (a <> zero ==> (n * a) / a == n)


ggen{|Stringnum|} n rnd
	= randomize (map fromInt [0,1,2,maxint]) rnd 4 (\rs -> flatten (map (\r -> let s = toString (abs r) in [Stringnum s,Stringnum (s+++s)]) rs))
genShow{|Stringnum|} sep p x rest
	= [toString x : rest]
derive bimap []
