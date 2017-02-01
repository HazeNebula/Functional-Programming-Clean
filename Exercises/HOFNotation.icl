module HOFNotation

import StdEnv
import StdDebug

f1 a b = a b

f2 a b c = a c (b c)

f3 a b = a (a b)

f4 a b c = [x \\ x <- [b..c] | a x]

f5 a b (c,d) = (a c,b d)

f6 = f5

f7 "-" = -
f7 "+" = +
f7 "*" = *
f7 "/" = /

Start = 42
