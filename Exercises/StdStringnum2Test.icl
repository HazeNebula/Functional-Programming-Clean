module StdStringnum2Test

/*	Test module StdStringnum2
	To work with Gast: 
		(*) use Environment 'Gast'
		(*) set Project Options to 'Basic Values Only'
*/

import StdStringnum2
import StdEnv
import gast

Start	= testn 1000
			(\a b -> 
		          negation_is_its_inverse             b
		       /\ abs_is_not_negative                 b
		       /\ abs_is_idempotent                   b
		       /\ zero_is_neutral_for_addition        b
		       /\ subtraction_of_self_is_zero         b
		       /\ subtraction_identities            a b
		       /\ one_is_neutral_for_multiplication   b
		       /\ one_is_neutral_for_division         b
		       /\ zero_is_zero_for_multiplication     b
		       /\ divide_by_self_is_identity          b
		       /\ though_shant_overflow             a b
		       /\ True
		    )

negation_is_its_inverse					:: Stringnum2 -> Property
negation_is_its_inverse a				= name "negation_is_its_inverse"
										       (~(~a) == a)

abs_is_not_negative						:: Stringnum2 -> Property
abs_is_not_negative a					= name "abs_is_not_negative"
										       (abs a >= zero)

abs_is_idempotent						:: Stringnum2 -> Property
abs_is_idempotent a						= name "abs_is_idempotent"
										       (abs (abs a) == abs a)

multiplication_by_sign_is_abs			:: Stringnum2 -> Property
multiplication_by_sign_is_abs a			= name "multiplication_by_sign_is_abs"
										       (a * fromInt (sign a) == abs a)

division_by_sign_is_abs					:: Stringnum2 -> Property
division_by_sign_is_abs a				= name "division_by_sign_is_abs"
										       (a <> zero ==> a / fromInt (sign a) == abs a)

zero_is_neutral_for_addition			:: Stringnum2 -> Property
zero_is_neutral_for_addition a			= name "zero_is_neutral_for_addition"
										       (zero + a == a && a == a + zero)

subtraction_of_self_is_zero				:: Stringnum2 -> Property
subtraction_of_self_is_zero a			= name "subtraction_of_self_is_zero"
										       (a - a == zero)

subtraction_identities					:: Stringnum2 Stringnum2 -> Property
subtraction_identities a b				= name "subtraction_identities"
										       (b - a == zero - (a - b) /\ (b - a) + a == b /\ a + (b - a) == b)

one_is_neutral_for_multiplication		:: Stringnum2 -> Property
one_is_neutral_for_multiplication a		= name "one_is_neutral_for_multiplication" 
										       (one * a == a && a == a * one)

one_is_neutral_for_division				:: Stringnum2 -> Property
one_is_neutral_for_division a			= name "one_is_neutral_for_division"
									       (a / one == a)

zero_is_zero_for_multiplication			:: Stringnum2 -> Property
zero_is_zero_for_multiplication a		= name "zero_is_zero_for_multiplication"
										       (zero * a == zero && zero == a * zero)

divide_by_self_is_identity				:: Stringnum2 -> Property
divide_by_self_is_identity a			= name "divide_by_self_is_identity"
										       (a <> zero ==> a / a == one)

though_shant_overflow					:: Stringnum2 Stringnum2 -> Property
though_shant_overflow n a				= name "though_shant_overflow"
										       (a <> zero ==> (n * a) / a == n)


ggen{|Stringnum2|} n rnd
	= randomize (map fromInt [0,1,-1,2,-2,maxint,minint]) rnd 7 (\rs -> flatten (map (\r -> let s = toString (abs r) in [Stringnum2 s,Stringnum2 (s+++s)]) rs))
genShow{|Stringnum2|} sep p x rest
	= [toString x : rest]
derive bimap []
