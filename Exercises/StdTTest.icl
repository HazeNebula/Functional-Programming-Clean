module StdTTest

/*	Test module StdTTest
	To work withmet Gast: 
		(*) use Environment 'Gast'
		(*) set Project Options to 'Basic Values Only'
*/

import StdT
import StdEnv
import gast

Start
			= testn 1000
				(\ i -> 
				       equality_is_symmetrical         i
				    /\ order_is_monotone               i
				    /\ negative_time_doesnt_exist      i
				    /\ conversion_to_Int_is_consistent i
				    /\ parse_print_is_consistent       i
				    /\ True
				)

t :: Int -> T
t x = fromInt x

equality_is_symmetrical				:: Int -> Property
equality_is_symmetrical i			= name "equality_is_symmetrical"
									       (t i == t i)

order_is_monotone					:: Int -> Property
order_is_monotone i					= name "order_is_monotone"
									       ((i <= i+1) ==> t i <= t (i+1))

negative_time_doesnt_exist			:: Int -> Property
negative_time_doesnt_exist i		= name "negative_time_doesnt_exist"
									       ((i + 1 >= i) ==> t i - t (i+1) == zero)

conversion_to_Int_is_consistent		:: Int -> Property
conversion_to_Int_is_consistent i	= name "conversion_to_Int_is_consistent"
									       ((abs i >= 0) ==> toInt (t (abs i)) == abs i)

parse_print_is_consistent			:: Int -> Property
parse_print_is_consistent i			= name "parse_print_is_consistent"
									       (fromString (toString (t i)) == t i)
