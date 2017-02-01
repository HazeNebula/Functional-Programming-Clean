implementation module StdRoman

import StdEnv
import StdDebug
import RomanNumeral

instance +			Roman where + _ _ = trace_n "instance + Roman not yet implemented" zero

instance -  		Roman where - _ _ = trace_n "instance - Roman not yet implemented" zero

instance zero		Roman where zero = abort "instance zero Roman not yet implemented"

instance *  		Roman where * _ _ = trace_n "instance * Roman not yet implemented" zero

instance /			Roman where / _ _ = trace_n "instance / Roman not yet implemented" zero

instance one		Roman where one = trace_n "instance one Roman not yet implemented" zero

instance ^			Roman where ^ _ _ = trace_n "instance ^ Roman not yet implemented" zero

instance abs		Roman where abs _ = trace_n "instance abs Roman not yet implemented" zero

instance sign		Roman where sign _ = trace_n "instance sign Roman not yet implemented" zero

instance ~			Roman where ~ _ = trace_n "instance ~ Roman not yet implemented" zero

instance ==			Roman where == _ _ = trace_n "instance == Roman not yet implemented" False

instance <  		Roman where < _ _ = trace_n "instance < Roman not yet implemented" False

instance isEven 	Roman where isEven _ = trace_n "instance isEven Roman not yet implemented" False

instance isOdd		Roman where isOdd _ = trace_n "instance isOdd Roman not yet implemented" False

class toRoman a :: !a -> Roman
instance toRoman	Char  where toRoman _ = trace_n "instance toRoman Char not yet implemented" zero

instance toRoman	Int   where toRoman _ = trace_n "instance toRoman Int not yet implemented" zero

instance toRoman	Real  where toRoman _ = trace_n "instance toRoman Real not yet implemented" zero

instance toRoman {#Char}  where toRoman _ = trace_n "instance toRoman String not yet implemented" zero

class fromRoman a :: !Roman -> a
instance fromRoman	Int   where fromRoman _ = trace_n "instance fromRoman Int not yet implemented" zero

instance fromRoman	Char  where fromRoman _ = trace_n "instance fromRoman Char not yet implemented" zero

instance fromRoman	Real  where fromRoman _ = trace_n "instance fromRoman Real not yet implemented" zero

instance fromRoman {#Char}where fromRoman _ = trace_n "instance fromRoman {#Char} not yet implemented" ""

// Additional functions for integer arithmetic: 

instance rem Roman where rem _ _ = trace_n "instance rem Roman not yet implemented" zero

instance gcd Roman where gcd _ _ = trace_n "instance gcd Roman not yet implemented" zero

instance lcm Roman where lcm _ _ = trace_n "instance lcm Roman not yet implemented" zero

