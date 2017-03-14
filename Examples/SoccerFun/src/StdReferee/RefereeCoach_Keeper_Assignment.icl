implementation module RefereeCoach_Keeper_Assignment

import StdEnvExt
import Referee
import Team_Opponent_Keeper_Assignment
import Team_Student_Keeper_Assignment

RefereeCoach_Keeper :: !FootballField -> Referee
RefereeCoach_Keeper field	= { name  = "RefereeCoach Keeper"
							  , brain = { memory = initMem
										, ai     = randomlessRefereeAI (brain field)
										}
							  , refActionPics    = []
							  }

::	Memory					= { first			:: !Bool
							  , practiceFailed	:: !Bool
							  , gameOver		:: !Bool
							  , showHowExit		:: !Maybe Exit
							  , timesGainedBy5	:: !Int
							  , practiceSucceed	:: !Bool
							  }
::	Exit					= { /*student			:: !Team
							  , computer		:: !Team
							  , */ball			:: !Position
							  , exit			:: !Position
							  }

initMem						:: Memory
initMem						= { first			= True
							  , practiceFailed	= False
							  , gameOver		= False
							  , showHowExit		= Nothing
							  , timesGainedBy5	= 0
							  , practiceSucceed	= False
							  }

brain :: !FootballField !(!RefereeInput,!Memory) -> (!RefereeOutput,!Memory)
brain field ({RefereeInput | playingTime=time,theBall=ballState,team1,team2},memory)
| memory.gameOver												// game over because of succes or failure
	= ([GameOver],memory)
| memory.practiceFailed && memory.practiceSucceed				// assignment failed but you had already succeeded
	= ([TellMessage "Failed, but you had already passed the test", DirectFreeKick studentHome ballKickoff],{memory & practiceFailed = False, showHowExit = Nothing})
| memory.practiceFailed											// assignment failed
	= ([TellMessage "Assignment failed, correct your team and try again"],{memory & gameOver = True})
| isEmpty (filter isKeeper studentTeam)							// check if there is a keeper
	= ([TellMessage ("No keeper in student team, please choose " <+++ base_TeamName_Student_Keeper)],{memory & gameOver = True})
# (refActions,memory)		= if (time <= minutes 0.5 && not memory.practiceSucceed)
							     ([TellMessage "Well Done! (but let's see how much longer you can gain control...)"], {memory & practiceSucceed=True})
						    	 ([],memory)
# (refActions,memory)		= if memory.first					// when this is the first round, reposition the ball
							     (refActions ++ [DirectFreeKick studentHome ballKickoff],{memory & first = False})
							     (refActions,memory)
| isJust memory.showHowExit										// The ball could pass, show how the ball could pass the keeper
	# exitInfo				= fromJust memory.showHowExit
	# directionToExit		= bearing zero exitInfo.ball exitInfo.exit	// move the ball towards exit
	# nextBallPos			= move_point {dx = m (cosinus directionToExit), dy = m (sinus directionToExit)} exitInfo.ball
	| studentHome == West && nextBallPos.px < scale -0.5 field.flength || studentHome == East && nextBallPos.px > scale 0.5 field.flength
							= ([],{memory & practiceFailed = True})
	| otherwise				= ([DirectFreeKick West nextBallPos],{memory & showHowExit = Just {exitInfo & ball=nextBallPos}})
| not memory.first && topExit.py >= scale 0.5 goal_width - spaceAllowed
	# sideStr				= if (studentHome == West) "left" "right"
	= ( [TellMessage ("Keeper left the " <+++ sideStr <+++ " side of the goal open. Exit ypos could be: " <+++ topExit.py)]
	  , {memory & showHowExit = Just {/*student=studentTeam,computer=compTeam,*/ball=theBall.ballPos.pxy,exit=topExit}}
	  )
| not memory.first && bottomExit.py <= scale -0.5 goal_width + spaceAllowed
	# sideStr				= if (studentHome == West) "right" "left"
	= ( [TellMessage ("keeper left the " <+++ sideStr <+++ " side of the goal open. Exit ypos could be: " <+++ bottomExit.py)]
	  , {memory & showHowExit = Just {/*student=studentTeam,computer=compTeam,*/ball=theBall.ballPos.pxy,exit=bottomExit}}
	  )
| otherwise					= (refActions,memory)
where
	(compTeam,studentTeam,studentHome)
							= if (stringStarts (nameOf team1) base_TeamName_Opponent_Keeper) (team1,team2,East) (team2,team1,West)
	ballKickoff				= if (studentHome == West)
							     {zero & px = scale -0.5 field.flength + penalty_area_depth}
							     {zero & px = scale  0.5 field.flength - penalty_area_depth}
	
/*	The keeper has to cover the entire width of the goal minus spaceAllowed at both sides in order to be succesfull enough for this assignment.
*/	spaceAllowed			= scale (1.0/6.0) goal_width
	theBall					= getFootball ballState (team1 ++ team2)
	keeper					= hd (filter isKeeper studentTeam)
	keeperPos				= keeper.pos
	ballDirection2keeper	= bearing zero theBall keeper
	(topReachKeeper,bottomReachKeeper)
							= getMaximumCatchPositions ballDirection2keeper
	
/*	Draws a line from football to keeper
	determines the two directions the keeper can jump to cover max area behind him
	gets the maximum distance the keeper can catch/jump
	returns the two positions past which the ball can pass the keeper
*/	getMaximumCatchPositions :: !Angle -> (!Position,!Position)
	getMaximumCatchPositions ballDirection2keeper
		# ballDirection2keeper			= if (ballDirection2keeper < rad (1.5*pi)) (ballDirection2keeper + rad (1.5*pi)) (ballDirection2keeper - rad (0.5*pi))
		# catchCorner1					= if (ballDirection2keeper < rad (1.5*pi)) (ballDirection2keeper + rad (1.5*pi)) (ballDirection2keeper - rad (0.5*pi))
		# catchCorner2					= if (ballDirection2keeper > rad (0.5*pi)) (ballDirection2keeper - rad (1.5*pi)) (ballDirection2keeper + rad (0.5*pi))
		# maxCatchDistance				= maxCatchReach keeper
		# maxPoint1						= {px=keeperPos.px + scale (sinus catchCorner1) maxCatchDistance, py=keeperPos.py + scale (~(cosinus catchCorner1)) maxCatchDistance }
		# maxPoint2						= {px=keeperPos.px + scale (sinus catchCorner2) maxCatchDistance, py=keeperPos.py + scale (~(cosinus catchCorner2)) maxCatchDistance }
		| maxPoint1.py < maxPoint2.py	= (maxPoint2,maxPoint1)
		| otherwise						= (maxPoint1,maxPoint2)
	
//	What if the keeper is passed could be the lowest position to let the ball exit the field
	direction2topReach					= bearing zero theBall topReachKeeper
	topExit								= getExitPos topReachKeeper theBall.ballPos.pxy direction2topReach  
//	What if the keeper is passed could be the lowest position to let the ball exit the field
	direction2bottomReach				= bearing zero theBall bottomReachKeeper
	bottomExit							= getExitPos bottomReachKeeper theBall.ballPos.pxy direction2bottomReach

	getExitPos :: !Position !Position !Angle -> Position
	getExitPos reachPos ballPos direction
										= while aintOut (move_point {dx=m (cosinus dir),dy=m (sinus dir)}) target
	where
		(dir,target)					= if (reachPos.px >= zero) (direction,reachPos) (oppositeDirection direction,ballPos)
		aintOut							= point_in_rectangle ({px = scale -0.5 field.flength, py = scale -0.5 field.fwidth}
										                     ,{px = scale  0.5 field.flength, py = scale  0.5 field.fwidth}
										                     )

oppositeDirection :: !Angle -> Angle
oppositeDirection d
| d > rad pi							= d - rad pi
| otherwise								= d + rad pi
