implementation module RefereeCoach_Slalom_Assignment

import Referee

RefereeCoach_Slalom				:: !FootballField -> Referee
RefereeCoach_Slalom field		= { name  = "RefereeCoach_Slalom"
								  , brain = { memory	= mkMemory
								            , ai		= randomlessRefereeAI (brain field)
								            }
								  , refActionPics		= []
								  }

::	Stage						= Begin | Slalom | Kick | End Success
instance == Stage where			== Begin    Begin		= True
								== Slalom   Slalom		= True
								== Kick     Kick		= True
								== (End s1) (End s2)	= s1 == s2
								== _        _			= False

::	Memory						= { positions	:: ![Position]		// all positions of student player, in reverse order
								  , stage		:: !Stage			// the current stage of the training
								  , home		:: Home				// home of student player, determined at Begin
								  }

mkMemory						:: Memory
mkMemory						= { positions	= []
								  , stage		= Begin
								  , home		= undef
								  }

slalom							:: Memory -> Memory
slalom memory					= {memory & stage = Slalom}

after_kick						:: Memory -> Memory
after_kick memory				= {memory & stage = Kick}

fail							:: Memory -> Memory
fail memory						= {memory & stage = End Fail}

ok								:: Memory -> Memory
ok memory						= {memory & stage = End Success}

position						:: Position Memory -> Memory
position p memory				= {memory & positions = [p:memory.positions]}

knownHome						:: Home Memory -> Memory
knownHome home memory			= {memory & home = home}

brain							:: !FootballField !(!RefereeInput,!Memory) -> (!RefereeOutput,!Memory)
//	Assignment has started. Check teams and determine home of student player.
brain field ({RefereeInput | team1,team2},memory=:{stage = Begin})
| not ok_teams					= ([TellMessage "Wrong teams selected."],fail memory)
| otherwise						= ([DirectFreeKick home ball],position student.pos (slalom (knownHome home memory)))
where
	(ok_teams,home)				= case (team1,team2) of
									([p],[_,_:_])			= (True, West)
									([_,_:_],[p])			= (True, East)
									otherwise				= (False,undef)
	student						= if (home == West) (hd team1) (hd team2)
	west_ball_pos				= {zero & px = scale -0.5 field.flength + penalty_area_depth}
	ball						= if (home == West) (mirror field west_ball_pos) west_ball_pos
//	Assignment has ended. Stop training session.
brain _ (_,memory=:{stage = End how})
								= ([TellMessage msg, GameOver],memory)
where
	msg							= if (how == Success) "Well done! Move on to next exercise."
								                      "Improve your assignment and try again."
//	Assignment is in slalom or kicking stage.
brain field ({RefereeInput | playingTime=time, theBall=ballState, team1, team2}, memory=:{home,positions,stage})
| time <= zero					= ([TellMessage "Out of time."],fail memory)							// time's up
| ballIsFree ballState && ballIsOut field ball															// ball is out
	# ball_pos					= ball.ballPos.pxy
	# (north,south)				= goal_poles field
	| not (isbetween ball_pos.py south north)															// student did not kick ball in goal
								= ([TellMessage "You should play the ball in the goal."],fail memory)
	| home == West && ball_pos.px < scale -0.5 field.flength + m 1.0 || 
	  home == East && ball_pos.px > scale  0.5 field.flength - m 1.0									// student kicked ball in wrong goal
								= ([TellMessage "You should play the ball in the other goal."],fail memory)
	| otherwise					= ([ContinueGame],ok memory)											// student ended exercise correctly
| isMoved action
	| compare_pos student.pos last_pos																	// student is moving in wrong direction
								= ([TellMessage "You're moving in the wrong direction."],fail memory)
	| any (\other -> dist other student < m 0.5) others													// student moves too close to opponents
								= ([TellMessage "Don't move so close to opponents."],fail memory)
	| not up_and_down			= ([TellMessage "You're not doing a slalom."],fail memory)				// student is not doing slalom
	| otherwise					= ([ContinueGame],memory)
| isKickedBall action
	| stage == Kick				= ([TellMessage "Don't kick the ball twice."],fail memory)				// kick the ball only once
	| otherwise					= ([ContinueGame],after_kick memory)
| otherwise						= ([TellMessage ("Illegal action. Perform only Move and KickBall. You did: " <+++ typeOfAction action)],fail memory)
where
	ball						= getFootball ballState (team1 ++ team2)
	(student,others)			= if (home == West) (hd team1,team2) (hd team2,team1)
	action						= fromJust student.effect
	last_pos					= hd positions
	new_positions				= [student.pos : positions]
	compare_px					= if (home == West) < >
	compare_pos pos1 pos2		= compare_px pos1.px pos2.px
	close_px    pos1 pos2		= abs (pos1.px - pos2.px) <= m 1.0
	line_up_others				= sortBy compare_pos (map (\{Footballer | pos} -> pos) others)
	close_encounters			= takeWhile (not o isEmpty) [filter (close_px other_pos) new_positions \\ other_pos <- line_up_others]
	up_and_down					= isAlternating 
								  [  map (\studentpos -> studentpos.py < other_pos.py) close_encounter
								  \\ close_encounter <- close_encounters
								   & other_pos       <- line_up_others
								  ]

ballIsOut						:: !FootballField !Football -> Bool
ballIsOut field ball			= not (point_in_rectangle ({px = scale -0.5 field.flength, py = scale -0.5 field.fwidth}
								                          ,{px = scale  0.5 field.flength, py = scale  0.5 field.fwidth}
								                          ) ball.ballPos.pxy)

typeOfAction					:: !FootballerEffect -> String
typeOfAction (Moved _ _)		= "Move"
typeOfAction (GainedBall _)		= "GainBall"
typeOfAction (KickedBall _)		= "KickBall"
typeOfAction (HeadedBall _)		= "HeadBall"
typeOfAction (Feinted _)		= "Feint"
typeOfAction (Tackled _ _ _)	= "Tackle"
typeOfAction (CaughtBall _)		= "Catch"

isAlternating					:: [[a]] -> Bool | Eq, ~ a
isAlternating sequences			= isEmpty sequences || all isSingleton singletons && map hd singletons == take n (iterate ~ (hd (hd singletons)))
where
	singletons					= map removeDup sequences
	n							= length sequences
