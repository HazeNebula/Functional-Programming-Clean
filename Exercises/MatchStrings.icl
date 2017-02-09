//	Exercise made by Jasper van den Bogart (4781686) and Niels van Nistelrooij (4713648)

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
is_sub s1 s2		= is_sub` s1 s2 0 0

is_sub`					:: String String Int Int -> Bool
is_sub` s1 s2 n1 n2
| n1 >= size s1			= True
| n2 >= size s2			= False
| s2.[n2] == s1.[n1]	= is_sub` s1 s2 ( n1 + 1 ) (n2 + 1 )
| otherwise				= is_sub` s1 s2 n1 ( n2 + 1 )

is_match			:: String String -> Bool
is_match s1 s2		= is_match` s1 s2 0 0

is_match`									:: String String Int Int -> Bool
is_match` s1 s2 n1 n2
| n1 >= size s1								= True
| n2 >= size s2								= False
| s1.[n1] == '*'							= is_match` s1 s2 ( n1 + 1 ) n2 || is_match` s1 s2 n1 ( n2 + 1 )
| s1.[n1] == '.' || s2.[n2] == s1.[n1]		= is_match` s1 s2 ( n1 + 1 ) ( n2 + 1 )
| otherwise									= False

Start
// ad-hoc tests:
//					= (head pink_floyd, tail pink_floyd)
//					= is_equal "" " "
//					= is_substring "hello" "hello"
//					= is_substring "there" pink_floyd
//					= is_substring "there" marillion
//					= is_sub "there" marillion
//					= is_sub "she and her" pink_floyd
//					= is_sub radiohead pink_floyd
//					= is_match "*.here*.here*." pink_floyd
					= is_match ".here.here." pink_floyd

pink_floyd			= "Is there anybody in there?"
marillion			= "Just for the record"
radiohead			= "There there"
