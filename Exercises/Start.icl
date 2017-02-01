// Exercise made by Jasper van den Bogart (4781686) and Niels van Nistelrooij (nnnnnnn)

/*
	1.	Nothing visible happens. However, the file that is not up to date (Start.icl)
		is recompiled.

	2.	A console window appears with the contents "Hello World!".

	3.	A new window opens which shows the type of every function.

	4.	expr1: the same as with expr0.
		expr2: the console window has 5 as the contents.
		expr3: the console window has 5.5 as the contents.
		expr4: type error: cannot unify types Int and Real.
		expr5: prints a list of the numbers 1 to 10.
		expr6: type error: cannot create list with different types
		expr7: 
		expr8: 
		expr9: 
		expr10: 
		expr11: 
 */

module Start

import StdEnv

Start = expr6
expr0 = "Hello World!"
expr1 = "Hello " +++ "World!"
expr2 = 5
expr3 = 5.5
// expr4 = 5 + 5.5
expr5 = [1..10]
expr6 = (expr1, expr2, expr3, expr5)
// expr7 = [expr1, expr2, expr3, expr5]
expr8 = [1, 3 .. 10]
expr9 = ['a' .. 'z']
expr10 = ['a', 'c' .. 'z']
expr11 = ['Hello World!']