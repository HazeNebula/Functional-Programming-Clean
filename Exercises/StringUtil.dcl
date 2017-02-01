definition module StringUtil

import StdString

/** This module defines a number of convenient String operations
*/

/** str <+ x = str +++ toString x
*/
(<+)  infixl:: !String !a -> String | toString a

/** x +> str = toString x +++ str
*/
(+>)  infixr:: !a !String -> String | toString a

/** x <+> y = toString x +++ toString y
*/
(<+>) infix :: !a !b      -> String | toString a & toString b

/** concat [str_1,...,str_n] creates a new string by glueing together str_1 ... str_n
*/
concat		:: ![String]  -> String

/** concatWith glue [str_1,...,str_n] creates a new string by glueing together str_1 ... str_n and 
    putting glue in between each of them
*/
concatWith	:: !String ![String] -> String
