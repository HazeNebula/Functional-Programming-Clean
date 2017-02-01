implementation module WordSleuth

import StdEnv
import StdDebug

Start							= wordSleuth puzzle words

puzzle							:: Puzzle
puzzle							= [['ECLL']
						          ,['PEIA']
						          ,['YSFZ']
						          ,['TANY']
						          ]

words							:: [Word]
words							= [['LAZY'],['LIST'],['TYPE'],['ZF']]

::	Puzzle	:== [[Char]]		// list of rows of characters (left to right, top to bottom)
::	Word	:== [Char]

wordSleuth :: Puzzle [Word] -> Word
wordSleuth _ _ = trace_n "wordSleuth not yet implemented" []

