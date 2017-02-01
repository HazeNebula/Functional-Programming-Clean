module VectorOverloadingTest

/*	Test module VectorOverloading
	To work with Gast: 
		(*) use Environment 'Gast'
		(*) set Project Options to 'Basic Values Only'
*/

import VectorOverloading
import StdEnv
import gast

Start
		= testn 1000
			(\v -> 
		          zero_is_neutral_for_addition      v
		       /\ zero_is_neutral_for_subtraction   v
		       /\ one_is_neutral_for_multiplication v
		       /\ one_is_neutral_for_division       v
		       /\ negation_is_idempotent            v
		       /\ add_then_subtract_yields_identity v
		       /\ subtract_then_add_yields_identity v
		       /\ True
		    )

:: BaseType
		:== Int
//		:== Real

zero_is_neutral_for_addition		:: (Vector2 BaseType) -> Property
zero_is_neutral_for_addition a		= name "zero_is_neutral_for_addition"
									       (zero + a == a && a == a + zero)

zero_is_neutral_for_subtraction		:: (Vector2 BaseType) -> Property
zero_is_neutral_for_subtraction a	= name "zero_is_neutral_for_subtraction"
									       (a - zero == a && a == ~ (zero - a))

one_is_neutral_for_multiplication	:: (Vector2 BaseType) -> Property
one_is_neutral_for_multiplication a	= name "one_is_neutral_for_multiplication" 
									       (one * a == a && a == a * one)

zero_is_zero_for_multiplication		:: (Vector2 BaseType) -> Property
zero_is_zero_for_multiplication a	= name "zero_is_zero_for_multiplication"
									       (zero * a == zero && zero == a * zero)

one_is_neutral_for_division			:: (Vector2 BaseType) -> Property
one_is_neutral_for_division a		= name "one_is_neutral_for_division"
									       (a / one == a)

negation_is_idempotent				:: (Vector2 BaseType) -> Property
negation_is_idempotent a			= name "negation_is_idempotent" 
									       (~ (~ a) == a)

add_then_subtract_yields_identity	:: (Vector2 BaseType) -> Property
add_then_subtract_yields_identity a	= name "add then subtract" ((a + a) - a == a)

subtract_then_add_yields_identity	:: (Vector2 BaseType) -> Property
subtract_then_add_yields_identity a	= name "subtract then add" ((zero - a - a) + a + a == zero)

derive genShow Vector2
derive ggen    Vector2
derive bimap   []
