implementation module StdT

import StdEnv
import StdDebug

::	T = {minutes :: Int, seconds :: Int}

instance ==			T where == t0 t1	= t0.minutes == t1.minutes && t0.seconds == t1.seconds

instance <			T where < t0 t1		= (t0.minutes * 60 + t0.seconds) < (t1.minutes * 60 + t1.seconds) 

instance zero		T where zero 		= {minutes = zero, seconds = zero}

instance +			T where + t0 t1		
									| (t0.seconds + t1.seconds) > 59	
									= {minutes = t0.minutes + t1.minutes + 1, seconds = (t0.seconds + t1.seconds) - 60}
									| otherwise
									= {minutes = t0.minutes + t1.minutes, seconds = t0.seconds + t1.seconds}

instance -			T where - t0 t1
									| (t0.seconds - t1.seconds) < 0
									= {minutes = t0.minutes - t1.minutes - 1, seconds = 60 + (t0.seconds - t1.seconds)}
									| otherwise
									= {minutes = t0.minutes - t1.minutes, seconds = t0.seconds - t1.seconds}

instance toInt		T where toInt t	= t.minutes * 60 + t.seconds

instance fromInt	T where fromInt int = {minutes = int / 60, seconds = int rem 60}

instance toString	T where toString t	= toString t.minutes +++ ":" +++ toString t.seconds

instance fromString	T where fromString string	= {minutes = toInt (string % (0, size string - 4)), seconds = toInt (string % (size string - 2, size string - 1))}

Start = toString {minutes = 8, seconds = 30}

