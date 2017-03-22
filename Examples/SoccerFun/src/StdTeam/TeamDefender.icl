implementation module TeamDefender

import StdEnv, StdIO, Footballer, FootballerFunctions

Team_Defender :: !Home !FootballField -> Team
Team_Defender home field
|	home==West				= westTeam
|	otherwise				= eastTeam
where
	eastTeam				= mirror field westTeam
	westTeam				= [keeper : fielders]
	clubname				= BASE_TEAMNAME_DEFENDER +++ if (home == West) "W" "E"
	keeper					= Defender clubname home field {zero & px=scale -0.5 field.flength} 1
	fielders				= [  Defender clubname home field {px=scale (-0.5*dx) field.flength,py=scale (0.5*dy) field.fwidth} nr
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
							  ,( 0.70, 0.75 )
							  ,( 0.70,-0.75 )
							  ,( 0.90, 0.30 )
							  ,( 0.90,-0.30 )
							  ]

BASE_TEAMNAME_DEFENDER		:: String
BASE_TEAMNAME_DEFENDER		= "DEFEND"

:: DefenderMemory				= { home :: !Home , startY :: Metre}

Defender :: !ClubName !Home !FootballField !Position !PlayersNumber -> Footballer
Defender club home field position nr
	= { playerID			= {clubName=club,playerNr=nr}
	  , name				= teamNames !! ( nr - 1 )
	  , length				= min_length
	  , pos					= position
	  , nose				= zero
	  , speed				= zero
	  , skills				= (Gaining, Kicking, Running)
	  , effect				= Nothing
	  , stamina				= max_stamina
	  , health				= max_health
	  , brain				= { memory = {home=home, startY = scale (0.5 * (snd (WEST_FIELDER_POSITIONS !! (nr - 2)))) field.fwidth}, ai = brains !! ( nr - 1 ) }
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
								placeholderBrain field,
								placeholderBrain field,
								placeholderBrain field,
								placeholderBrain field,
								placeholderBrain field,
								placeholderBrain field,
								defenderBrain field,
								defenderBrain field,
								defenderBrain field,
								defenderBrain field,
								defenderBrain field
							]

placeholderBrain					:: !FootballField !( !BrainInput, !DefenderMemory ) -> ( !BrainOutput, !DefenderMemory )
placeholderBrain _ ( _, memory )	= ( Move { direction = zero, velocity = ms 0.0 } zero, memory )

				  
defenderBrain				:: !FootballField !(!BrainInput,!DefenderMemory) -> (!BrainOutput,!DefenderMemory)
defenderBrain field (input=:{referee,football,others,me}, memory=:{home, startY})
| ballIsGainedBy me.playerID football
= (kick_ball field (input,memory), new_memory)
| dist me ball < maxGainReach me
= (GainBall, new_memory)
| me.pos.py > startY + (m 8.0)
= (move_down, new_memory)
| me.pos.py < startY - (m 8.0)
= (move_up, new_memory)
| if (home == West) (me.pos.px > m 5.0) (me.pos.px < m -5.0)
= if (home == West) (move_left, new_memory) (move_right, new_memory)
| any (\ x	= ballIsGainedBy x football) [x.playerID \\ x <- (me opponents others)]
	| if (home == West) (ball.ballPos.pxy.px < m 5.0) (ball.ballPos.pxy.px > m -5.0)
		| if (home == West) (ball.ballPos.pxy.px < me.pos.px) (ball.ballPos.pxy.px > me.pos.px)
		= (sprint_to_ball, new_memory2)
		| otherwise	
			| dist me ball < m 9.0	
			= (move_to_ball, new_memory2)
			| otherwise
			= (move_to_path_ball field (input, memory), new_memory2)
	| otherwise
	= (Move zero zero, new_memory)
| any (\ x	= ballIsGainedBy x football) [x.playerID \\ x <- (me team others)]
	| dist me (closest_opponent me) < (m 5.0)
	= (move_away_opponent, new_memory)
	| otherwise
	= (move_away_ball, new_memory)
| otherwise
	| dist me ball < m 9.0
	= (move_to_ball, new_memory2)
	| otherwise
	= (move_to_path_ball field (input, memory), new_memory)
where
	ball				= getBall input
	move_to_ball 		= Move {direction = (bearing zero me ball), velocity = ms 3.0 + ball.ballSpeed.vxy.velocity} (bearing me.nose me ball)
	sprint_to_ball		= Move {direction = (bearing zero me ball), velocity = ms 999999999999999999999.9} (bearing me.nose me ball)
	new_memory			= {home = if (any isEndHalf referee) (other home) home, startY = scale (0.5 * (snd (WEST_FIELDER_POSITIONS !! (me.playerID.playerNr - 2)))) field.fwidth}
	new_memory2			= {home = if (any isEndHalf referee) (other home) home, startY = ball.ballPos.pxy.py}
	closest_opponent		:: Footballer -> Footballer
	closest_opponent pl	= (pl opponents others) !! (snd (minList [(dist pl x, i) \\ x <- (pl opponents others) & i <- [0..]]))
	move_down			= Move {direction = (degree 270), velocity = ms 3.0} (bearing me.nose me ball)
	move_up				= Move {direction = (degree 90), velocity = ms 3.0} (bearing me.nose me ball)
	move_left			= Move {direction = (degree 180), velocity = ms 3.0} (bearing me.nose me ball)
	move_right			= Move {direction = (degree 0), velocity = ms 3.0} (bearing me.nose me ball)
	move_away_opponent	= Move {direction = ((bearing zero me (closest_opponent me)) + (degree 180)), velocity = ms 5.0} (bearing me.nose me ball)
	move_away_ball		= Move {direction = ((bearing zero me ball) + (degree 180)), velocity = ms 6.0} (bearing me.nose me ball)


kick_ball	:: FootballField (BrainInput, DefenderMemory) -> BrainOutput
kick_ball field (input=:{others,me,referee,football}, memory=:{home})
| any (\ x = if (home == West) (x.pos.px > me.pos.px) (x.pos.px < me.pos.px)) (me team others)
	| any (\ x	= (abs ((bearing zero me closest_team_mate) - (bearing zero me x))) < (degree 20)) [x \\ x <- (me opponents others) | if (home == West) (x.pos.px < closest_team_mate.pos.px) (x.pos.px > closest_team_mate.pos.px)]
	= kick_ball field ({others = removeMember closest_team_mate others, me = me, football = football, referee = referee}, memory)
	| otherwise
	= KickBall {vxy = {direction = (bearing zero me closest_team_mate), velocity = ms 10.0 + (ms (toReal (dist me closest_team_mate) * 1.5))}, vz = ms 2.0}
| (me team others) == []
= if (home == West) move_right move_left
| otherwise
= if (home == West) move_left move_right
where
	closest_team_mate 		= (me team others) !! (snd (minList [(dist me x, i) \\ x <- (me team others) & i <- [0..]| if (home == West) (x.pos.px > me.pos.px) (x.pos.px < me.pos.px)]))
	move_left				= Move {direction = (degree 180), velocity = ms 6.0} (bearing me.nose me {px = scale -0.5 field.flength, py = m 0.0})
	move_right				= Move {direction = (degree 0), velocity = ms 6.0} (bearing me.nose me {px = scale 0.5 field.flength, py = m 0.0})


move_to_path_ball	:: FootballField (BrainInput, DefenderMemory) -> BrainOutput
move_to_path_ball field (input=:{me}, memory=:{home})
= Move {direction = (bearing zero me ball_position_to), velocity = (ms 3.0 + ball.ballSpeed.vxy.velocity)} (bearing me.nose me ball_position_to)
where
	ball_position_to	:: Position
	ball_position_to	= {px = xCor, py = yCor}
	xCor				= (m ((cos (toReal ball.ballSpeed.vxy.direction)) * (toReal ball.ballSpeed.vxy.velocity))) + ball.ballPos.pxy.px
	yCor				= (m ((sin (toReal ball.ballSpeed.vxy.direction)) * (toReal ball.ballSpeed.vxy.velocity))) + ball.ballPos.pxy.py
	ball				= getBall input













