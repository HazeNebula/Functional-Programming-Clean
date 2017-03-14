definition module StdIOExt

/**	Collection of functions that extend functionality of StdIO.
*/

import StdIO

/** makeSound path ioSt
		plays a sound file, located at path. If no such file is found, the function
		aborts.
*/
makeSound :: !String -> (IOSt .ps) -> IOSt .ps
