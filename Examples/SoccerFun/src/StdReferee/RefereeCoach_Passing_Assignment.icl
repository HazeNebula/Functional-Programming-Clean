implementation module RefereeCoach_Passing_Assignment

import StdEnvExt
import Referee
import Team_Opponent_Passing_Assignment

RefereeCoach_Passing :: !FootballField -> Referee
RefereeCoach_Passing field	= { name  = "RefereeCoach Passing"
							  , brain = { memory = initMem
							            , ai     = randomlessRefereeAI (brain field)
							            }
							  , refActionPics    = []
							  }

:: Memory = { first				:: !Bool
			, kickedBy1 		:: !Bool			// PA: oh dear, you should use [Bool], or better: if in sequence: Int
			, kickedBy2 		:: !Bool
			, kickedBy3 		:: !Bool
			, kickedBy4 		:: !Bool
			, kickedBy5 		:: !Bool
			, kickedBy6 		:: !Bool
			, pos1				:: !Position		// PA: dito, but now [Position]
			, pos2				:: !Position
			, pos3				:: !Position
			, pos4				:: !Position
			, pos5				:: !Position
			, pos6				:: !Position
			, practiceFailed	:: !Bool
			, gameOver			:: !Bool	
			}

initMem :: Memory
initMem   = { first				= True
			, kickedBy1			= False
			, kickedBy2			= False
			, kickedBy3			= False
			, kickedBy4			= False
			, kickedBy5			= False
			, kickedBy6			= False
			, pos1				= zero
			, pos2				= zero
			, pos3				= zero
			, pos4				= zero
			, pos5				= zero
			, pos6				= zero
			, practiceFailed	= False
			, gameOver			= False
			}

brain :: !FootballField !(!RefereeInput,!Memory) -> (!RefereeOutput,!Memory)
brain field ({RefereeInput | theBall=ballState, team1, team2}, memory)
| memory.gameOver												// game over because of succes or failure
	= ([GameOver],memory)
| memory.practiceFailed											// assignment has failed
	= ([TellMessage "Assignment failed, correct your team and try again"],{memory & gameOver = True})
| length players <> 6											// Check if there are indeed six fielders (to prevent run time errors)
	= ([TellMessage "Number of fielders should be six. Correct your team and try again."],{memory & gameOver = True})
# (refActions,memory)	= if memory.first						// when first, alter ball pos and remember all positions
							([DirectFreeKick studentHome ballKickoff],{updateAllPos memory & first = False})
							([],memory)
# playersMovedToFar		= getPlayersThatMovedTooFar memory		// check if players did not move more than 10 meters from their starting positions
| not (isEmpty playersMovedToFar)
	= (refActions ++ getMessagesForTooFarPlayers playersMovedToFar,{memory & practiceFailed = True})
| not memory.kickedBy1											// first player has not kicked the ball yet
	# (more,memory)		= checkIfPlayerKickedBall 1 memory
	= (refActions++more,memory)
| not memory.kickedBy2											// second player has not kicked the ball yet
	# (more,memory)		= checkIfPlayerKickedBall 2 memory
	= (refActions++more,memory)
| not memory.kickedBy3											// third player has not kicked the ball yet
	# (more,memory)		= checkIfPlayerKickedBall 3 memory
	= (refActions++more,memory)
| not memory.kickedBy4											// fourth player has not kicked the ball yet
	# (more,memory)		= checkIfPlayerKickedBall 4 memory
	= (refActions++more,memory)
| not memory.kickedBy5											// fifth player has not kicked the ball yet
	# (more,memory)		= checkIfPlayerKickedBall 5 memory
	= (refActions++more,memory)
| not memory.kickedBy6											// sixth player has not kicked the ball yet
	# (more,memory)		= checkIfPlayerKickedBall 6 memory
	= (refActions++more,memory)
| ballIsGoingInWrongDirection									// ball is going in the wrong direction
	= (refActions ++ [TellMessage "ball should be played towards the goal"],{memory & practiceFailed = True})
| ballBehindLine												// ball is behind the line
	| theBall.ballPos.pz > goal_height							// ball is over
		= (refActions ++ [TellMessage "Try shooting less high, the ball went over"],{memory & practiceFailed = True})
	| isbetween theBall.ballPos.pxy.py south_pole north_pole
		= (refActions ++ [TellMessage "Well Done! Move on to the next assignment!"],{memory & gameOver = True})
	| otherwise
		= (refActions ++ [TellMessage "Ball is out, try shooting in the goal next time"],{memory & practiceFailed = True})
| theBall.ballSpeed.vxy.velocity == zero						// ball stopped moving
	= (refActions ++ [TellMessage "Try shooting harder, the ball stopped moving forwards"],{memory & practiceFailed = True})
| otherwise														// ball may be on its way to the goal
	= (refActions,memory)
where
	theBall										= getFootball ballState (team1 ++ team2)
	(compTeam,(studentTeam,studentHome))		= if (stringStarts (nameOf team1) base_TeamName_Opponent_Passing) (team1,(team2,East)) (team2,(team1,West))
	ballKickoff									= {zero & px = if (studentHome == West) (scale -0.5 field.flength + penalty_area_depth) (scale 0.5 field.flength - penalty_area_depth)}
	getMessagesForTooFarPlayers					= map (\fb -> TellMessage ("Your player with number " <+++ fb.playerID.playerNr <+++ " moved further than 10 meters"))
	(north_pole,south_pole)						= goal_poles field
	
	players										= filter isFielder studentTeam
	stud1										= players!!0
	stud2										= players!!1
	stud3										= players!!2
	stud4										= players!!3
	stud5										= players!!4
	stud6										= players!!5
	stud1fa										= getAction stud1.playerID
	stud2fa										= getAction stud2.playerID
	stud3fa										= getAction stud3.playerID
	stud4fa										= getAction stud4.playerID
	stud5fa										= getAction stud5.playerID
	stud6fa										= getAction stud6.playerID
	
	getAction :: !FootballerID -> Maybe FootballerEffect
	getAction playerID							= case [fb.effect \\ fb <- team1 ++ team2 | identify_player playerID fb] of
													[effect:_]	= effect
													_			= Nothing
	
	ballIsGoingInWrongDirection					= if (studentHome == West) > < theBall.ballSpeed.vxy.direction (rad (0.5*pi)) &&
												  if (studentHome == East) < > theBall.ballSpeed.vxy.direction (rad (1.5*pi))			
	ballBehindLine								= abs theBall.ballPos.pxy.px > scale 0.5 field.flength
			
	updateAllPos :: !Memory -> Memory
	updateAllPos memory							= {memory & pos1 = stud1.pos
												          , pos2 = stud2.pos
												          , pos3 = stud3.pos
												          , pos4 = stud4.pos
												          , pos5 = stud5.pos
												          , pos6 = stud6.pos
												  }
	
	getPlayersThatMovedTooFar :: !Memory -> [Footballer]
	getPlayersThatMovedTooFar memory
		= [fb \\ fb <- players & pos <- [memory.pos1,memory.pos2,memory.pos3,memory.pos4,memory.pos5,memory.pos6] | dist fb pos > m 10.0]
								
	checkIfPlayerKickedBall :: !Int !Memory -> (![RefereeAction],!Memory)
	checkIfPlayerKickedBall i memory
		# (studfa,stud)							= case i of
													1 -> (stud1fa,stud1)
													2 -> (stud2fa,stud2)
													3 -> (stud3fa,stud3)
													4 -> (stud4fa,stud4)
													5 -> (stud5fa,stud5)
													6 -> (stud6fa,stud6)
		| perhaps isKickedBall studfa			= ([],                                                           updateMemory i memory)
		| perhaps isCaughtBall studfa			= ([Hands stud.playerID, ReprimandPlayer stud.playerID RedCard],{memory & practiceFailed = True})
		| otherwise								= ([],                                                           memory)
	where
		updateMemory :: !Int !Memory -> Memory
		updateMemory 1 m	= {m & kickedBy1 = True}
		updateMemory 2 m	= {m & kickedBy2 = True}
		updateMemory 3 m	= {m & kickedBy3 = True}
		updateMemory 4 m	= {m & kickedBy4 = True}
		updateMemory 5 m	= {m & kickedBy5 = True}
		updateMemory 6 m	= {m & kickedBy6 = True}
