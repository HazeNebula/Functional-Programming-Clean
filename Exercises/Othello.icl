module Othello

import StdEnv
import StdDebug
import StdGameTree

/*	Types required for the game logic:
*/
::	GameState			// define your game state here
::	Profit				// define your profit type here
::	Player				= PC | Human

instance ~  Player  	where (~)  Human               = PC
                    	      (~)  PC                  = Human
instance == Player		where (==) Human     Human     = True
			                  (==) PC        PC        = True
			                  (==) _         _         = False

initGameState			:: Player -> GameState
initGameState player	= abort "initGameState not yet implemented"

moves					:: GameState -> [GameState]
moves current			= abort "moves not yet implemented"

worth					:: GameState -> Profit
worth current			= abort "worth not yet implemented"

Start					= nextmoves 2 worth moves (initGameState PC)
