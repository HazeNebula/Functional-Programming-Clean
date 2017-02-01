implementation module StdIOMonad

//	This module gives StdFile a monadic structure

import StdList, StdString, StdTuple, StdFile, StdMisc
import StdDebug
import StdMonad
import StdMaybeMonad

:: IO a								  = IO (*WorldSt -> *(a,*WorldSt))
:: *WorldSt							  // define a suitable state for the IO monad

:: Void								  = Void
:: Filemode							  = Read | Write
:: Filename							:== String

:: Filehandle						  // define a suitable implementation for Filehandle

instance toInt Filemode where
	toInt Read						= FReadText
	toInt Write						= FWriteText

//  IO is a monad:
instance return IO where
	return _ = abort "instance return IO not yet implemented"

instance >>= IO where
	>>= _ _ = abort "instance >>= IO not yet implemented"

//	perform monadic action on the world:
doIO								:: (IO a) *World -> (a,*World)
doIO _ world = trace_n "doIO not yet implemented" (undef,world)

//	read line from console:
read								:: IO String
read = abort "read not yet implemented"

//	write line to console:
write								:: String -> IO Void
write _ = abort "write not yet implemented"

//	open file with given filename and mode:
open								:: Filename Filemode -> IO (Maybe Filehandle)
open _ _ = abort "open not yet implemented"

//	close the file that has been opened:
close								:: Filehandle -> IO Bool
close _ = abort "close not yet implemented"

//	determine reading end of file:
eof									:: Filehandle -> IO Bool
eof _ = abort "eof not yet implemented"

//	read a line from a file:
readline							:: Filehandle -> IO (Maybe String)
readline _ = abort "readline not yet implemented"

//	write a line to a file:
writeline							:: String Filehandle -> IO Bool
writeline _ _ = abort "writeline not yet implemented"

