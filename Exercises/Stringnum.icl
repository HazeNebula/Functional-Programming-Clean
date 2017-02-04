implementation module Stringnum

import StdEnv
import StdDebug

Start = equal "5" "5"

isAStringnum			:: String -> Bool
isAStringnum s			= isAStringNum` s ( size s - 1 )

isAStringNum`			:: String Int -> Bool
isAStringNum` s n
| n > 0					= if ( isDigit s.[n] ) ( isAStringNum` s ( n - 1 ) ) False
| n == 0				= if ( isDigit s.[n] ) True False
| otherwise				= abort "Invalid index."

plus					:: String String -> String
plus a b				= toString ( toInt a + toInt b )

decrement				:: String String -> String
decrement a b			= toString ( toInt a - toInt b )

times					:: String String -> String
times a b				= toString ( toInt a * toInt b )

divide					:: String String -> String
divide a b				= toString ( toInt a / toInt b )

smaller					:: String String -> Bool
smaller a b				= toInt a < toInt b

bigger					:: String String -> Bool
bigger a b				= toInt a > toInt b

equal					:: String String -> Bool
equal a b				= toInt a == toInt b

