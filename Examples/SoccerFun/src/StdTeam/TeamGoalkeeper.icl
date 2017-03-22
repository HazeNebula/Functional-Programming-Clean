implementation module TeamGoalkeeper

import StdEnv, StdIO, Footballer, FootballerFunctions

Team_Goalkeeper :: !Home !FootballField -> Team
Team_Goalkeeper home field
|	home==West				= westTeam
|	otherwise				= eastTeam
where
	eastTeam				= mirror field westTeam
	westTeam				= [keeper : fielders]
	clubname				= BASE_TEAMNAME_GOALKEEPER +++ if (home == West) "W" "E"
	keeper					= Goalkeeper clubname home field {zero & px=scale -0.5 field.flength} 1
	fielders				= [  Goalkeeper clubname home field {px=scale (-0.5*dx) field.flength,py=scale (0.5*dy) field.fwidth} nr
							  \\ (dx,dy) <- WEST_FIELDER_POSITIONS
							   & nr      <- [2..]
							  ]

WEST_FIELDER_POSITIONS		:: [( Real, Real )]
WEST_FIELDER_POSITIONS		= [( 0.20, 0.40 )
							  ,( 0.20,-0.40 )
							  ,( 0.23, 0.00 )
							  ,( 0.50, 0.45 )
							  ,( 0.50,-0.45 )
							  ,( 0.60, 0.00 )
							  ,( 0.70, 0.35 )
							  ,( 0.70,-0.35 )
							  ,( 0.90, 0.05 )
							  ,( 0.90,-0.05 )
							  ]

BASE_TEAMNAME_GOALKEEPER		:: String
BASE_TEAMNAME_GOALKEEPER		= "KEEPER"

:: GoalkeeperMemory				= { home :: !Home , prevRefAction :: RefereeAction}

Goalkeeper :: !ClubName !Home !FootballField !Position !PlayersNumber -> Footballer
Goalkeeper club home field position nr
	= { playerID			= {clubName=club,playerNr=nr}
	  , name				= teamNames !! ( nr - 1 )
	  , length				= max_length
	  , pos					= position
	  , nose				= zero
	  , speed				= zero
	  , skills				= (Rotating, Kicking, Catching)
	  , effect				= Nothing
	  , stamina				= max_stamina
	  , health				= max_health
	  , brain				= { memory = {home=home, prevRefAction = ContinueGame}, ai = brains !! ( nr - 1 ) }
	  }
	where
		teamNames			= [
								"Goatee Protector",
								"Old Man",
								"Evil Jewelry",
								"The Cleaner",
								"Placeholder",
								"Placeholder",
								"Placeholder",
								"Placeholder",
								"Placeholder",
								"Placeholder",
								"Placeholder"
							]
		brains				= [
								goalkeeperBrain field,
								placeholderBrain field,
								placeholderBrain field,
								placeholderBrain field,
								placeholderBrain field,
								placeholderBrain field,
								placeholderBrain field,
								placeholderBrain field,
								placeholderBrain field,
								placeholderBrain field,
								placeholderBrain field
							]

placeholderBrain					:: !FootballField !( !BrainInput, !GoalkeeperMemory ) -> ( !BrainOutput, !GoalkeeperMemory )
placeholderBrain _ ( _, memory )	= ( Move { direction = zero, velocity = ms 0.2 } zero, memory )

	
goalkeeperBrain				:: !FootballField !(!BrainInput,!GoalkeeperMemory) -> (!BrainOutput,!GoalkeeperMemory)
goalkeeperBrain field (input=:{referee,football,others,me}, memory=:{home,prevRefAction})	
| isGoalKick prevRefAction
	| dist goal ball > m 60.0 || any isOwnBallIllegally referee
	= (Move zero zero, new_memory)
	| dist me ball < maxKickReach me
	= (kick_ball field (input, memory), {home = if (any isEndHalf referee) (other home) home, prevRefAction = ContinueGame})
	| otherwise
	= (move_to_ball 3.5, {home = if (any isEndHalf referee) (other home) home, prevRefAction = GoalKick home})
| ballIsGainedBy me.playerID football
= (kick_ball field (input, memory), new_memory)
| dist me ball < maxCatchReach me
	| dist goal ball < m 13.0
	= (CatchBall, new_memory)
	| otherwise
	= (GainBall, new_memory)
| if (home == West) (me.pos.px > ball.ballPos.pxy.px) (me.pos.px < ball.ballPos.pxy.px)
= (move_to_goal 999999999.9, new_memory)
| dist goal ball < m 11.0
= (move_between_ball_goal field (input, memory), new_memory)
| dist me ball < (dist ball closest_opponent_to_ball - m 1.0) && dist goal ball < m 18.0
= (move_to_ball (toReal (ms 2.0 + (scale 0.8 ball.ballSpeed.vxy.velocity))), new_memory)
| abs ((dist me ball) - (scale 6.0 (dist goal me))) < m 5.0
= (move_between_ball_center_goal field (input, memory), new_memory)
| otherwise
= if (dist me ball > scale 6.0 (dist goal me))	(move_to_ball 2.0, new_memory)	(move_to_goal 2.0, new_memory)
where
	ball 		= getBall input
	goal		= centerOfGoal home field
	new_memory 	= {home = if (any isEndHalf referee) (other home) home, prevRefAction = if (any isGoalKick referee) (GoalKick home) ContinueGame}
	closest_team_member = (me team others) !! (snd (minList [(dist me x.pos, i) \\ x <- (me team others) & i <- [0..] | if (home == West) (x.pos.px > me.pos.px) (x.pos.px < me.pos.px)]))
	closest_opponent_to_ball 	= (me opponents others) !! (snd (minList [(dist ball x.pos, i) \\ x <- (me opponents others) & i <- [0..]]))
	move_to_ball v				= Move {direction = (bearing zero me ball), velocity = ms v} (bearing me.nose me ball)
	move_to_goal v				= Move {direction = (bearing zero me goal), velocity = ms v} (bearing me.nose me ball)
	best_team_mate				= (me team others) !! (snd (maxList [(dist x.pos (closest_opponent x).pos, i) \\ x <- (me team others) & i <- [0..]]))
	closest_opponent pl	= (pl opponents others) !! (snd (minList [(dist pl x.pos, i) \\ x <- (pl  opponents others) & i <- [0..]]))


kick_ball	:: FootballField (BrainInput, GoalkeeperMemory) -> BrainOutput
kick_ball field (input=:{others,me,referee,football}, memory=:{home})
| any (\ x = if (home == West) (x.pos.px > me.pos.px) (x.pos.px < me.pos.px)) (me team others)
	| any (\ x	= (abs ((bearing zero me closest_team_mate) - (bearing zero me x))) < (degree 20)) [x \\ x <- (me opponents others) | if (home == West) (x.pos.px < closest_team_mate.pos.px) (x.pos.px > closest_team_mate.pos.px)]
	= kick_ball field ({others = removeMember closest_team_mate others, me = me, football = football, referee = referee}, memory)
	| otherwise
	= KickBall {vxy = {direction = (bearing zero me closest_team_mate), velocity = ms 10.0 + (ms (toReal (dist me closest_team_mate) * 1.5))}, vz = ms 2.0}
| otherwise
= KickBall {vxy = {direction = (bearing zero me mate_with_most_space), velocity = ms 10.0 + (ms (toReal (dist me mate_with_most_space) * 1.5))}, vz = ms 2.0}
where
	closest_team_mate 		= (me team others) !! (snd (minList [(dist me x, i) \\ x <- (me team others) & i <- [0..]| if (home == West) (x.pos.px > me.pos.px) (x.pos.px < me.pos.px)]))
	mate_with_most_space	= (me team others) !! (snd (maxList [(dist x (closest_opponent x), i) \\ x <- (me team others) & i <- [0..]]))
	closest_opponent pl		= (me opponents others) !! (snd (minList [(dist pl x, i) \\ x <- (me opponents others) & i <- [0..]]))




move_between_ball_goal		:: FootballField (BrainInput, GoalkeeperMemory) -> BrainOutput
move_between_ball_goal field (input=:{me}, memory=:{home}) 
| if (home == West) (ball.ballSpeed.vxy.direction > (degree 90)) (ball.ballSpeed.vxy.direction < (degree 90))
	| 2.0 * pi + toReal (bearing zero ball ball_position_to) > 2.0 * pi + toReal (bearing zero ball me)
	= (Move {direction = angle_down, velocity = ms 2.0 + (scale 0.8 ball.ballSpeed.vxy.velocity)} (bearing me.nose me ball))
	| 2.0 * pi + toReal (bearing zero ball ball_position_to) < 2.0 * pi + toReal (bearing zero ball me)
	= Move {direction = angle_up, velocity = ms 2.0 + (scale 0.8 ball.ballSpeed.vxy.velocity)} (bearing me.nose me ball)
	| otherwise
	= Move zero (bearing me.nose me ball)
| otherwise
= Move zero (bearing me.nose me ball)
where
	ball 			= getBall input
	angle_down 		= (bearing zero me ball) - ((degree 90) - ((bearing zero ball ball_position_to) - (bearing zero ball me)))
	angle_up		= (bearing zero me ball) + ((degree 90) - ((bearing zero ball me) - (bearing zero ball ball_position_to)))
	ball_position_to		:: Position
	ball_position_to	= {px = if (home == West) (scale -0.5 field.flength) (scale 0.5 field.flength), py = m ((sin (toReal ball.ballSpeed.vxy.direction)) * (((0.5 * (toReal field.flength)) - (abs (toReal ball.ballPos.pxy.px))) / (cos (toReal ball.ballSpeed.vxy.direction))))}




move_between_ball_center_goal		:: FootballField (BrainInput, GoalkeeperMemory) -> BrainOutput
move_between_ball_center_goal field (input=:{me}, memory=:{home})
| 2.0 * pi + toReal (bearing zero ball goal) > 2.0 * pi + toReal (bearing zero ball me)
= (Move {direction = angle_down, velocity = ms 2.0 + (scale 0.8 ball.ballSpeed.vxy.velocity)} (bearing me.nose me ball))
| 2.0 * pi + toReal (bearing zero ball goal) < 2.0 * pi + toReal (bearing zero ball me)
= Move {direction = angle_up, velocity = ms 2.0 + (scale 0.8 ball.ballSpeed.vxy.velocity)} (bearing me.nose me ball)
| otherwise
= Move zero (bearing me.nose me ball)
where
	ball 			= getBall input
	goal			= centerOfGoal home field
	angle_down 		= (bearing zero me ball) - ((degree 90) - ((bearing zero ball goal) - (bearing zero ball me)))
	angle_up		= (bearing zero me ball) + ((degree 90) - ((bearing zero ball me) - (bearing zero ball goal)))
	

















