definition module Domino

::	Tile	:== (Int,Int)		// (1..N,1..N)
::	Snake	:== [Tile]

N			:== 2

/*	all_snakes max
 *	returns all snakes of length max for a double-N game.
 */
all_snakes :: Int -> [Snake]
