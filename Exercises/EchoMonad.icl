module EchoMonad

import StdString		// explicit import from StdEnv to avoid nameclash with StdFunc / StdIOMonad
import StdDebug
import StdIOMonad
import StdMaybeMonad

Start :: *World -> (Void,*World)
Start world = trace_n "Start not yet implemented" (Void,world)

