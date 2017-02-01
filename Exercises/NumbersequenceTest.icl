module NumbersequenceTest

/*	Test module Numbersequence
	For working with Gast:
		(*) Use Environment 'Gast'
		(*) Set Project Options to 'Basic Values Only'
*/

import StdEnv
import Numbersequence
import gast

Start					= testn 1000
							(\n ->     charsInt_is_inverse n
							        /\ True
							)

charsInt_is_inverse		:: Int -> Property
charsInt_is_inverse n	= name "charsInt_is_inverse"
							(n > 0 ==> charsInt (intChars n) == n)

//	intChars and digitChar come from the lecture notes, paragraph 3.2.4:
intChars				:: (Int -> [Char])
intChars				= map digitChar
				           o reverse
				           o map (\z = z rem 10)
				           o takeWhile ((<>) 0)
				           o iterate (\x = x / 10)

digitChar				:: Int -> Char
digitChar n
| 0 <= n && n <= 9		= toChar (n + toInt '0')
