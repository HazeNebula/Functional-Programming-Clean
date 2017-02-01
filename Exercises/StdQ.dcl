definition module StdQ

import StdOverloaded

::	Q

instance ==			Q		// equality of rational numbers
instance <  		Q		// order of rational numbers

instance +			Q		// addition of rational numbers
instance -  		Q		// subtraction of rational numbers
instance zero		Q		// the neutral element of addition (x+zero = zero+x = x)

instance *  		Q		// multiplication of rational numbers
instance /			Q		// division of rational numbers
instance one		Q		// the neutral element of multiplication (x*one = one*x = x)

instance abs		Q		// the absolute value of the rational number
instance sign		Q		// the sign of the rational number
instance ~			Q		// change the sign of the rational number

isInt :: Q -> Bool			// test whether the rational number represents a whole number
instance toInt  Q			// convert the rational number to an Int (with possible rounding)
instance toReal Q			// convert the rational number to a Real (approximately)

class toQ a :: a -> Q
instance toQ Int			// convert the Int to a rational number
instance toQ Real			// convert the Real to a rational number
instance toQ (Int,Int)		// convert a (t, n) to a rational number with t as the numerator and n as the denominator
instance toQ (Int,Int,Int)	// convert a (x, t, n) to a rational number with x as the scalar, t as the numerator and n as the denominator
instance toString Q			// print a rational number as "s(x+t/n)" in such a way that:
							// q = x + t/n (as math dictates)
							// 0 < abs (t/n) < 1
							// s = "" als q>=0; s = -1 otherwise
							// ex.: toString (toQ  11 2) = "(5+1/2)"
							// ex.: toString (toQ -11 2) = "-(5+1/2)"
							
							// if abs (t/n) == 0, only show x
							// ex: toString (toQ 10 2) = "5"
