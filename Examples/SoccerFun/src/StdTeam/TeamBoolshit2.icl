implementation module TeamBoolshit2

import StdEnv, StdIO, Footballer, FootballerFunctions

Team_Boolshit2 :: !Home !FootballField -> Team
Team_Boolshit2 home field
|	home==West				= westTeam
|	otherwise				= eastTeam
where
	eastTeam				= mirror field westTeam
	westTeam				= [keeper : fielders]
	clubname				= BASE_TEAMNAME_BOOLSHIT2 +++ if (home == West) "W" "E"
	keeper					= Boolshit2 clubname home field {zero & px=scale -0.5 field.flength} 1
	fielders				= [  Boolshit2 clubname home field {px=scale (-0.5*dx) field.flength,py=scale (0.5*dy) field.fwidth} nr
							  \\ (dx,dy) <- west_fielder_positions
							   & nr      <- [2..]
							  ]
	where
		west_fielder_positions		= [( 0.20, 0.40 )
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

BASE_TEAMNAME_BOOLSHIT2		:: String
BASE_TEAMNAME_BOOLSHIT2		= "Boolshit"

:: Bool2Memory				= { home :: !Home }

Boolshit2 :: !ClubName !Home !FootballField !Position !PlayersNumber -> Footballer
Boolshit2 club home field position nr
	= { playerID			= { clubName = club, playerNr = nr}
	  , name				= toString nr
	  , length				= max_length
	  , pos					= position
	  , nose				= zero
	  , speed				= zero
	  , skills				= ( Running, Rotating, Tackling )
	  , effect				= Nothing
	  , stamina				= max_stamina
	  , health				= max_health
	  , brain				= { memory = { home = home }, ai = brains !! ( nr - 1 ) }
	  }
	  where
	  	brains				= [
								placeholderBrain field,
								tackleBrain field,
								tackleBrain field,
								tackleBrain field,
								tackleBrain field,
								tackleBrain field,
								placeholderBrain field,
								placeholderBrain field,
								placeholderBrain field,
								placeholderBrain field,
								placeholderBrain field
							]

placeholderBrain					:: !FootballField !( !BrainInput, !Bool2Memory ) -> ( !BrainOutput, !Bool2Memory )
placeholderBrain _ ( _, memory )	= ( Move zero zero, memory )

tackleBrain				:: !FootballField !(!BrainInput,!Bool2Memory) -> (!BrainOutput,!Bool2Memory)
tackleBrain field (input=:{referee, football, others, me}, memory=:{home})
	| ballIsGainedBy me.playerID football	= ( KickBall { vxy = { direction = direction_absolute goal, velocity = mach_25 }, vz = mach_25 }, new_memory )
	| ( dist me ball ) < ( maxGainReach me )	= ( GainBall, new_memory )
	| ( dist me my_opponent ) > ( m 1.0 )	= ( Move { direction = direction_absolute my_opponent, velocity = mach_25 } ( direction_relative my_opponent ), new_memory )
	| otherwise	= ( Tackle me.playerID mach_25, new_memory )
	where
		new_memory	= { home = if (any isEndHalf referee) (other home) home }
		ball	= getBall input
		goal	= centerOfGoal ( other home ) field
		my_team	= me team others
		my_opponents	= me opponents others
		my_opponent_index	= if ( ( me.playerID.playerNr - 1 ) < length my_opponents ) ( me.playerID.playerNr - 1 ) ( length my_opponents - 1 )
		my_opponent	= my_opponents !! my_opponent_index
		direction_absolute target	= bearing zero me target
		direction_relative target	= bearing me.nose me target
		mach_25	= ms 30626.0
