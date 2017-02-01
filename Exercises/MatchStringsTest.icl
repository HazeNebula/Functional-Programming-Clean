module MatchStringsTest

/*	Test module MatchStrings
	To work with Gast:
		(*) use Environment 'Gast'
		(*) put Project Options to 'Basic Values Only'
*/

import StdEnv
import MatchStrings
import gast

Start = testn 1000
	(\s t -> 
		   property_true_for_equal_arguments   is_equal     s
		/\ property_false_for_bigger_arguments is_equal     s
		/\ property_false_for_bigger_arguments is_substring s
		/\ property_true_for_equal_arguments   is_substring s
		/\ property_true_for_equal_start       is_substring s t
		/\ property_true_for_equal_center      is_substring s t
		/\ property_true_for_equal_end         is_substring s t
		/\ property_false_for_bigger_arguments is_sub       s
		/\ property_true_for_equal_arguments   is_sub       s
		/\ property_true_for_equal_start       is_sub       s t
		/\ property_true_for_equal_center      is_sub       s t
		/\ property_true_for_equal_end         is_sub       s t
		/\ property_true_for_extensions        is_sub       s t
		/\ kleenes_matches_always                           s
		/\ dots_match_same_length                           s
		/\ dots_cant_match_more                             s
		/\ True
	)

property_false_for_bigger_arguments :: (String String -> Bool) String -> Property
property_false_for_bigger_arguments f a
	= name "property_false_for_bigger_arguments" (not (f (a +++ ".") a))

property_true_for_equal_arguments :: (String String -> Bool) String -> Property
property_true_for_equal_arguments f a
	= name "property_true_for_equal_arguments" (f a a)

property_true_for_equal_start :: (String String -> Bool) String String -> Property
property_true_for_equal_start f a b
	= name "property_true_for_equal_start" (f a (a +++ b))

property_true_for_equal_center :: (String String -> Bool) String String -> Property
property_true_for_equal_center f a b
	= name "property_true_for_equal_center" (f a (b +++ a +++ b))

property_true_for_equal_end :: (String String -> Bool) String String -> Property
property_true_for_equal_end f a b
	= name "property_true_for_equal_end" (f a (b +++ a))

property_true_for_extensions :: (String String -> Bool) String String -> Property
property_true_for_extensions f a extra
	= name "property_true_for_extensions" 
						   ((extra <> "") ==> f a (extend_string extra a))

extend_string :: String String -> String
extend_string extra string
	= toString (flatten [[c:cs] \\ c <-: string])
where
	cs = fromString extra

kleenes_matches_always :: String -> Property
kleenes_matches_always s
	= name "kleenes_matches_always"
	   (ForEach [{# '*' \\ _ <- [1..n]} \\ n <- [1..10]] (flip is_match s))

dots :: String -> String
dots s
	= {# '.' \\ _ <-: s}

dots_match_same_length :: String -> Property
dots_match_same_length s
	= name "dots_match_same_length"
	   (is_match (dots s) s)

dots_cant_match_more :: String -> Property
dots_cant_match_more s
	= name "dots_cant_match_more"
		(not (is_match ("." +++ dots s) s) && 
		not (is_match (dots s) (s+++".")))
