implementation module WC

import StdEnv
import StdDebug
import Words
import SimpleFileIO

Start			:: *World -> (Int,*World)
Start world		= wc "WC.icl" world

wc				:: String *env -> (Int,*env) | FileSystem env
wc _ env = trace_n "wc not yet implemented" (zero,env)

