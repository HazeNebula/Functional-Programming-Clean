implementation module Stringnum2

import StdEnv
import StdDebug
import Stringnum

Start = equal2 "-5" "-5"

isAStringnum					:: String -> Bool
isAStringnum s					= isAStringNum` s ( size s - 1 )

isAStringNum`					:: String Int -> Bool
isAStringNum` s n
| n > 0							= if ( isDigit s.[n] ) ( isAStringNum` s ( n - 1 ) ) False
| n == 0						= if ( isDigit s.[n] || s.[n] == '-' ) True False
| otherwise						= abort "Invalid index."

isNegative						:: String -> Bool
isNegative s					= s.[0] == '-'

absolute						:: String -> String
absolute s						= if ( isNegative s ) ( s % ( 0, size s - 1 ) ) s

changeSign						:: String -> String
changeSign s					= if ( isNegative s ) ( absolute s ) ( "-" +++ s )

plus2							:: String String -> String
plus2 a b						= toString ( toInt a + toInt b )

decrement2						:: String String -> String
decrement2 a b					= toString ( toInt a - toInt b )

times2							:: String String -> String
times2 a b						= toString ( toInt a * toInt b )

divide2							:: String String -> String
divide2 a b						= toString ( toInt a / toInt b )

smaller2						:: String String -> Bool
smaller2 a b					= toInt a < toInt b

bigger2							:: String String -> Bool
bigger2 a b						= toInt a > toInt b

equal2							:: String String -> Bool
equal2 a b						= toInt a == toInt b

