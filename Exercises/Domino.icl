implementation module Domino

import StdEnv
import StdDebug

::	Tile  :== (Int,Int)		// (1..N,1..N)
::	Snake :== [Tile]

N					  :== 2

tiles n					= [ (pip1,pip2)
						  \\ pip1 <- [0 .. n]
						   , pip2 <- [pip1 .. n]
						  ]

Start					= [(s,'\n') \\ s <- all_snakes 6]

all_snakes				:: Int -> [Snake]
all_snakes _ = trace_n "all_snakes not yet implemented" []

