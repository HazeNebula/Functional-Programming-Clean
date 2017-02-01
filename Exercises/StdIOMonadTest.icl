module StdIOMonadTest

import StdArray, StdInt, StdString	// explicit imports of StdEnv modules to avoid name-clash with StdFunc and StdIOMonad
import StdDebug
import StdIOMonad

Start :: *World -> (Void,*World)
Start world = doIO my_monadic_program world

my_monadic_program :: IO Void
my_monadic_program
	= write "Enter a file name: " >>= \_ ->
      read                        >>= \filename ->
      open (filename%(0,size filename-2)) Read >>= \maybe_filename ->
      case maybe_filename of
           Nothing
               = write ("Could not open '" +++ filename +++ "'.\n") >>= \_ ->
                 return Void
           Just filehandle
               = show_content filehandle

show_content :: Filehandle -> IO Void
show_content filehandle
	= eof filehandle >>= \end ->
	  if end
	     (return Void)
	     (readline filehandle >>= \maybe_line ->
	      case maybe_line of
	          Nothing   = return Void
	          Just line = write line >>= \_ -> show_content filehandle
	     )
