module Start

import StdEnv
import StdDebug

Start = expr0

expr0 = "Hello World!"
expr1 = "Hello " +++ "World!"
expr2 = 5
expr3 = 5.5
expr4 = 5 + 5.5
expr5 = [1 .. 10]
expr6 = (expr1,expr2,expr3,expr5)
expr7 = [expr1,expr2,expr3,expr5]
expr8 = [1,3 .. 10]
expr9 = ['a' .. 'z']
expr10 = ['a','c' .. 'z']
expr11 = ['Hello World!']

