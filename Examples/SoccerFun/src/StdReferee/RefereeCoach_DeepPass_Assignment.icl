implementation module RefereeCoach_DeepPass_Assignment

import StdEnvExt
import Referee
import Team_Opponent_DeepPass_Assignment

RefereeCoach_DeepPass :: !FootballField -> Referee
RefereeCoach_DeepPass field	= { name  = "RefereeCoach DeepPass"
							  , brain = { memory = initMem
							            , ai     = randomlessRefereeAI (brain field)
							            }
							  , refActionPics    = []
							  }

::	Memory = { first			:: !Bool
			 , practiceFailed	:: !Bool
			 , gameOver			:: !Bool
			 , ballKicked		:: !Bool
			 , ballGained		:: !Bool
			 , positions		:: !(!Position,!Position)
			 }

initMem :: Memory
initMem   = { first				= True
			, practiceFailed	= False
			, gameOver			= False
			, ballKicked		= False
			, ballGained		= False
			, positions			= (zero,zero)
			}

brain :: !FootballField !(!RefereeInput,!Memory) -> (!RefereeOutput,!Memory)
brain field ({RefereeInput | theBall=ballState,team1,team2},memory)
| memory.gameOver																	// game over because of succes or failure
	= ([GameOver],memory)
| memory.practiceFailed																// assignment has failed
	= ([TellMessage "Assignment failed, correct your team and try again."],stop memory)
| length (filter isFielder studentTeam) <> 2										// check if there are indeed two fielders in the students team
	= ([TellMessage "There should be two fielders in student team."],stop memory)
# (refActions,memory)						= if memory.first						// when first, alter ball pos and remember all positions
											     ([DirectFreeKick studentHome ballKickoff],{updateAllPos memory & first = False})
											     ([],memory)
# playersMovedToFar							= getPlayersThatMovedTooFar memory
| not (isEmpty playersMovedToFar)													// check if players did not move more than 10 meters from their starting positions
	= (refActions ++ getMessagesForTooFarPlayers playersMovedToFar,fail memory)
| ballGainedByComputer
	= (refActions ++ [TellMessage ("Computer gained the ball")],fail memory)
| not memory.ballKicked
	| perhaps isKickedBall kickerAction		= (refActions,kick memory)
	| perhaps isTackled    kickerAction		= (refActions ++ [TackleDetected kickerID, ReprimandPlayer kickerID RedCard],fail memory)
	| perhaps isTackled    gainerAction		= (refActions ++ [TackleDetected gainerID, ReprimandPlayer gainerID RedCard],fail memory)
	| otherwise								= (refActions,memory)
| not memory.ballGained
 	| perhaps isTackled    kickerAction		= (refActions ++ [TackleDetected kickerID, ReprimandPlayer kickerID RedCard],fail memory)
	| perhaps isTackled    gainerAction		= (refActions ++ [TackleDetected gainerID, ReprimandPlayer gainerID RedCard],fail memory)
	| perhaps isGainedBall gainerAction		= (refActions ++ [TellMessage "Well Done! Move on to the next assignment!"], stop memory)
	| otherwise								= (refActions,memory)
| otherwise
	= (refActions,memory)
where
	(compTeam,studentTeam,studentHome)		= if (stringStarts (nameOf team1) base_TeamName_Opponent_DeepPass) (team1,team2,East) (team2,team1,West)
	(kicker,  gainer)						= let fielders = filter isFielder studentTeam in (fielders!!0,fielders!!1)
	(kickerID,gainerID)						= (kicker.playerID,gainer.playerID)
	kickerAction							= getAction kickerID
	gainerAction							= getAction gainerID
	ballKickoff								= if (studentHome == West)
											     {py=scale -0.05 field.fwidth, px=scale  0.06 field.flength}
											     {py=scale  0.05 field.fwidth, px=scale -0.06 field.flength}
	ballGainedByComputer					= any (\fb -> ballIsGainedBy fb.playerID ballState) compTeam
	
	fail memory								= {memory & practiceFailed = True}
	stop memory								= {memory & gameOver       = True}
	kick memory								= {memory & ballKicked     = True}

	getAction :: !FootballerID -> Maybe FootballerEffect
	getAction playerID						= case [fb.effect \\ fb <- team1 ++ team2 | identify_player playerID fb] of
												[effect:_]	= effect
												_			= Nothing
			
	updateAllPos :: !Memory -> Memory
	updateAllPos memory						= {memory & positions = (kicker.pos,gainer.pos)}
	
	getPlayersThatMovedTooFar :: !Memory -> [Footballer]
	getPlayersThatMovedTooFar {positions=(kicker_pos,gainer_pos)}
											= [fb \\ (fb,pos) <- [(kicker,kicker_pos),(gainer,gainer_pos)] | dist fb pos > m 10.0]
		
	getMessagesForTooFarPlayers				= map (\fb -> TellMessage ("Your player with number " <+++ fb.playerID.playerNr <+++ " moved further than 10 meters"))
