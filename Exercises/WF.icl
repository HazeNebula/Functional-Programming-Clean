implementation module WF

import StdEnv
import StdDebug
import WordFrequency
import SimpleFileIO
import FrequencylistGUI

Start :: *World -> *World
Start world
	# (fl,world)			= wf "WF.icl" world
	= showFrequencylist2 fl world

wf :: String *env -> ([(String,Int)],*env) | FileSystem env
wf _ env = trace_n "wf not yet implemented" ([],env)

