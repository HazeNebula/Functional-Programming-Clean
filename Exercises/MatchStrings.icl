implementation module MatchStrings

import StdEnv
import StdDebug

head				:: String -> Char
head s
| s == ""			= abort "Couldn't return head because s is an empty string."
| otherwise			= s.[0]

tail				:: String -> String
tail s
| s == ""			= abort "Couldn't return tail because s is an empty string."
| size s == 1		= ""
| otherwise			= s % ( 1, size s - 1 )

is_equal			:: String String -> Bool
is_equal s1 s2		= s1 == s2

is_substring			:: String String -> Bool
is_substring "" _		= True
is_substring _ ""		= False
is_substring s1 s2 		= is_substring` s1 s2 0

is_substring`					:: String String Int -> Bool
is_substring` s1 s2 n
| ( n + size s1 ) <= size s2	= if ( s1 == s2 % ( n, n + size s1 - 1 ) ) True ( is_substring` s1 s2 ( n + 1 ) )
| otherwise						= False

is_sub				:: String String -> Bool
is_sub _ _ = trace_n "is_sub not yet implemented" False

is_match			:: String String -> Bool
is_match _ _ = trace_n "is_match not yet implemented" False

Start
// ad-hoc tests:
//					= (head pink_floyd, tail pink_floyd)
//					= is_equal "" " "
					= is_substring "hello" "hello"
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
