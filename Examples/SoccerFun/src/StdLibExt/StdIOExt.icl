implementation module StdIOExt

/**	Collection of functions that extend functionality of StdIO.
*/
import	StdEnv
import	StdIO
import	clCCall_12
from	iostate import appIOToolbox

/** makeSound path ioSt
		plays a sound file, located at path. If no such file is found, the function
		aborts.
*/
makeSound :: !String -> (IOSt .l) -> IOSt .l
makeSound file		= appIOToolbox (osSound file)
where
	osSound :: !String !*OSToolbox -> *OSToolbox
	osSound file tb
		# (ok,tb)	= winPlaySound file tb
		| ok		= tb
		| otherwise	= abort ("makeSound: unable to play sound file" +++ file +++ ".\n")
