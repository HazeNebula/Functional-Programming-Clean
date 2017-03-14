implementation module TeamMiniEffie

import StdEnv, StdIO, Footballer, FootballerFunctions

Team_MiniEffies :: !Home !FootballField -> Team
Team_MiniEffies home field
|	home==West				= westTeam
|	otherwise				= eastTeam
where
	eastTeam				= mirror field westTeam
	westTeam				= [keeper : fielders]
	clubname				= base_TeamName_MiniEffie +++ if (home == West) "W" "E"
	keeper					= MiniEffie clubname home field {zero & px=scale -0.5 field.flength} 1
	fielders				= [  MiniEffie clubname home field {px=scale (-0.5*dx) field.flength,py=scale (0.5*dy) field.fwidth} nr
							  \\ (dx,dy) <- west_positions_fielders
							   & nr      <- [2..]
							  ]
	west_positions_fielders	= [(0.20, 0.40)
							  ,(0.20,-0.40)
							  ,(0.23, 0.00)	
							  ,(0.50, 0.45)
							  ,(0.50,-0.45)
							  ,(0.60, 0.00)
							  ,(0.70, 0.35)
							  ,(0.70,-0.35)
							  ,(0.90, 0.05)
							  ,(0.90,-0.05)
							  ]
							  
base_TeamName_MiniEffie		:: String
base_TeamName_MiniEffie		= "MiniEffies"							  

:: MiniMemory				= { home :: !Home }

MiniEffie :: !ClubName !Home !FootballField !Position !PlayersNumber -> Footballer
MiniEffie club home field position nr
	= { playerID			= {clubName=club,playerNr=nr}
	  , name				= "MiniF." <+++ nr
	  , length				= min_length
	  , pos					= position
	  , nose				= zero
	  , speed				= zero
	  , skills				= (Running, Kicking, Rotating)
	  , effect				= Nothing
	  , stamina				= max_stamina
	  , health				= max_health
	  , brain				= { memory = {home=home}, ai = minibrain field }
	  }	  

minibrain :: !FootballField !(!BrainInput,!MiniMemory) -> (!BrainOutput,!MiniMemory)
minibrain field (input=:{referee,me}, memory=:{home})
| i_am_close_to_the_ball
	| i_can_see goal		= (kick_the_ball,     new_memory)
	| otherwise				= (turn_to_face goal, new_memory)
| i_can_see ball			= (run_to_the_ball,   new_memory)
| otherwise					= (turn_to_face ball, new_memory)	
where
	new_memory 				= {home=if (any isEndHalf referee) (other home) home}
	my_direction			= me.nose
	ball					= getBall input
	goal					= centerOfGoal (other home) field
	
	i_am_close_to_the_ball	= dist me ball < maxKickReach me
	i_can_see pos			= abs (bearing my_direction me pos) < rad (0.05*pi)

	run_to_the_ball			= Move {direction=bearing zero me ball,velocity=speed_of_light} zero
	turn_to_face pos		= Move zero (bearing my_direction me pos)
	kick_the_ball 			= KickBall {vxy={direction=my_direction,velocity=speed_of_light}, vz=ms 1.0}
	speed_of_light			= ms 299792458.0
