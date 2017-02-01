implementation module StringUtil

import StdList, StdString

(<+) infixl	:: !String !a -> String | toString a
(<+) txt a	= txt +++ toString a

(+>) infixr	:: !a !String -> String | toString a
(+>) a txt	= toString a +++ txt

(<+>) infix :: !a !b -> String | toString a & toString b
(<+>) a b	= toString a +++ toString b

concat		:: ![String] -> String
concat strs	= foldr (+++) "" strs

concatWith	:: !String ![String] -> String
concatWith _ []  = ""
concatWith _ [a] = toString a
concatWith glue strs = foldr (\a str -> toString a +++ glue +++ str) (last strs) (init strs)
