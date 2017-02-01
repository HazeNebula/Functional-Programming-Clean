implementation module Frequencylist

import StdEnv
import StdDebug

/* Without GUI: */
Start						= frequencylist text

/* With GUI:
import FrequencylistGUI
Start world					= showFrequencylist2 list world */

list						= sort (frequencylist text)
text						= ['Hello world! Here I am!']

frequencylist				:: [a] -> [(a,Int)] | == a
frequencylist _ = trace_n "frequencylist not yet implemented" []

