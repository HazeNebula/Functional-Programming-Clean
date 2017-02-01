implementation module MatchStrings

import StdEnv
import StdDebug

head				:: String -> Char
head _ = trace_n "head not yet implemented" zero

tail				:: String -> String
tail _ = trace_n "tail not yet implemented" ""

is_equal			:: String String -> Bool
is_equal _ _ = trace_n "is_equal not yet implemented" False

is_substring		:: String String -> Bool
is_substring _ _ = trace_n "is_substring not yet implemented" False

is_sub				:: String String -> Bool
is_sub _ _ = trace_n "is_sub not yet implemented" False

is_match			:: String String -> Bool
is_match _ _ = trace_n "is_match not yet implemented" False

Start
// ad-hoc tests:
					= (head pink_floyd, tail pink_floyd)
//					= is_equal "" " "
//					= is_substring "hello" "hello"
//					= is_substring "there" pink_floyd
//					= is_substring "there" marillion
//					= is_sub "there" marillion
//					= is_sub "she and her" pink_floyd
//					= is_sub radiohead pink_floyd
//					= is_match "*.here*.here*." pink_floyd
//					= is_match ".here.here." pink_floyd

pink_floyd			= "Is there anybody in there?"
marillion			= "Just for the record"
radiohead			= "There there"
