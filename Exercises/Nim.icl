implementation module Nim

import StdEnv
import StdDebug
import StdGameTree

/*	Types required for the game logic:
*/
::	GameState			// define your game state here

::	Player				=	PC | Human

::	Profit				// define your profit type here

instance ~  Player  	where (~)  Human               = PC
                    	      (~)  PC                  = Human
instance == Player		where (==) Human     Human     = True
	                          (==) PC        PC        = True
	                          (==) _         _         = False

initGameState			:: Player -> GameState
initGameState _ = trace_n "initGameState not yet implemented" (abort "GameState not yet implemented")

moves					:: GameState -> [GameState]
moves _ = trace_n "moves not yet implemented" []

worth					:: GameState -> Profit
worth _ = trace_n "worth not yet implemented" (abort "Profit not yet implemented")

Start					= nextmoves 2 worth moves (initGameState PC)
