definition module StdRoman

import RomanNumeral
import StdOverloaded

instance +				Roman
instance -  			Roman
instance zero			Roman

instance *  			Roman
instance /				Roman
instance one			Roman


instance ^				Roman
instance abs			Roman
instance sign			Roman
instance ~				Roman

instance ==				Roman
instance <  			Roman
instance isEven 		Roman	// True if arg1 is an even number
instance isOdd			Roman	// True if arg1 is an odd  number

class toRoman a :: !a -> Roman
instance toRoman		Char
instance toRoman		Int
instance toRoman		Real
instance toRoman		{#Char}

class fromRoman a :: !Roman -> a
instance fromRoman		Int
instance fromRoman		Char
instance fromRoman		Real
instance fromRoman		{#Char}

instance rem Roman
instance gcd Roman
instance lcm Roman
