definition module StdIOMonad

//	This module creates monad-style wrappers for a number of StdFile functions

import StdMonad, StdMaybeMonad

:: IO a
:: Void       = Void
:: Filemode   = Read | Write
:: Filename :== String
:: Filehandle

//	perform monadic action on the world:
doIO		:: (IO a) *World -> (a,*World)

//  IO is a monad:
instance return IO
instance >>=    IO

//	read line from console:
read		:: IO String

//	write line to console:
write		:: String -> IO Void

//	open file with given filename and mode:
open		:: Filename Filemode -> IO (Maybe Filehandle)

//	close the file that has been opened:
close		:: Filehandle -> IO Bool

//	determine reading end of file:
eof			:: Filehandle -> IO Bool

//	read a line from a file:
readline	:: Filehandle -> IO (Maybe String)

//	write a line to a file:
writeline	:: String Filehandle -> IO Bool
