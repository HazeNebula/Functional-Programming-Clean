implementation module TeamVanguard

import StdEnv, StdIO, Footballer, FootballerFunctions
import StdDebug

Team_Vanguard :: !Home !FootballField -> Team
Team_Vanguard home field
|	home==West				= westTeam
|	otherwise				= eastTeam
where
	eastTeam				= mirror field westTeam
	westTeam				= [keeper : fielders]
	clubname				= BASE_TEAMNAME_VANGUARD +++ if (home == West) "W" "E"
	keeper					= Vanguard clubname home field {zero & px=scale -0.5 field.flength} 1
	fielders				= [  Vanguard clubname home field {px=scale (0.5*dx) field.flength,py=scale (0.5*dy) field.fwidth} nr
							  \\ (dx,dy) <- WEST_FIELDER_POSITIONS
							   & nr      <- [2..]
							  ]

WEST_FIELDER_POSITIONS		:: [( Real, Real )]
WEST_FIELDER_POSITIONS		= [( -0.20, 0.40 )
							  ,( -0.20,-0.40 )
							  ,( -0.23, 0.00 )
							  ,( -0.50, 0.45 )
							  ,( -0.50,-0.45 )
							  ,( -0.60, 0.00 )
							  ,( -0.70, 0.35 )
							  ,( -0.70,-0.35 )
							  ,( -0.90, 0.05 )
							  ,( -0.90,-0.05 )
							  ]

BASE_TEAMNAME_VANGUARD		:: String
BASE_TEAMNAME_VANGUARD		= "VANGUARD"

:: VanguardMemory				= { home :: Home, pass :: Bool, playerHome :: Position, return_home :: Bool }

Vanguard :: !ClubName !Home !FootballField !Position !PlayersNumber -> Footballer
Vanguard club home field position nr
	= { playerID			= {clubName=club,playerNr=nr}
	  , name				= teamNames !! ( nr - 1 )
	  , length				= scale 0.5 ( min_length + max_length )
	  , pos					= position
	  , nose				= zero
	  , speed				= zero
	  , skills				= (Dribbling, Kicking, Gaining)
	  , effect				= Nothing
	  , stamina				= max_stamina
	  , health				= max_health
	  , brain				= { memory = { home = home, pass = False, playerHome = position, return_home = False}, ai = brains !! ( nr - 1 ) }
	  }
	where
		teamNames			= [
								"1",
								"2",
								"3",
								"4",
								"5",
								"6",
								"7",
								"8",
								"9",
								"10",
								"11"
							]
		brains				= [
								placeholderBrain field,
								vanguardBrain field,
								vanguardBrain field,
								vanguardBrain field,
								vanguardBrain field,
								vanguardBrain field,
								vanguardBrain field,
								vanguardBrain field,
								vanguardBrain field,
								vanguardBrain field,
								vanguardBrain field
							]
							

placeholderBrain					:: !FootballField !( !BrainInput, !VanguardMemory ) -> ( !BrainOutput, !VanguardMemory )
placeholderBrain _ ( _, memory )	= ( Move zero zero, memory )

vanguardBrain						:: !FootballField !(!BrainInput, !VanguardMemory) -> (!BrainOutput, !VanguardMemory)
vanguardBrain field ( input =: {referee, football, others, me}, memory =: {home} )
	| ballIsGainedBy me.playerID football
		| memory.pass
			| length find_pass > 0	= ( pass ( hd find_pass ), new_memory )
			| otherwise				= ( Move zero zero, new_memory )
		| length find_pass > 0	= ( turn_to_pass ( hd find_pass ), { new_memory & pass = True } )
		| otherwise	= ( Move zero zero, new_memory )
	| otherwise	= ( Move zero zero, new_memory )
	where
		//	Memory
		new_memory	:: VanguardMemory
		new_memory	= { home = if ( any isEndHalf referee ) ( other home ) home, pass = False, playerHome = memory.playerHome, return_home = False }
		
		//	Objects
		my_team	:: [Footballer]
		my_team	= me team others
		my_opponents	:: [Footballer]
		my_opponents	= me opponents others
		
		//	Calculating passes
		angle_teammate_opponent								:: Footballer Footballer Angle -> Real
		angle_teammate_opponent teammate opponent offset	= toReal ( ( bearing zero me teammate + offset ) - ( bearing zero me opponent ) )
		dist_opponent_teammatetrajectory							:: Footballer Footballer Angle -> Real
		dist_opponent_teammatetrajectory teammate opponent offset	= ( sin ( angle_teammate_opponent teammate opponent offset ) ) * ( toReal ( dist me opponent ) )
		is_trajectory_safe							:: Footballer Footballer Angle -> Bool
		is_trajectory_safe teammate opponent offset	= ( dist_opponent_teammatetrajectory teammate opponent offset ) > ( toReal ( maxKickReach opponent ) ) || ( dist me teammate ) < ( dist me opponent )
		is_pass_safe_given_opponent						:: Footballer Footballer -> Bool
		is_pass_safe_given_opponent teammate opponent	= and [is_trajectory_safe teammate opponent ( degree n ) \\ n <- [-3..3]]
		is_pass_safe			:: Footballer -> Bool
		is_pass_safe teammate	= and [is_pass_safe_given_opponent teammate opponent \\ opponent <- my_opponents]
		find_pass	:: [Footballer]
		find_pass	= [( teammate ) \\ teammate <- my_team | is_pass_safe teammate && ( dist me teammate ) > ( m 2.0 )]
		
		turn_to_pass			:: Footballer -> FootballerAction
		turn_to_pass teammate	= Move zero ( bearing me.nose me teammate )
		pass			:: Footballer -> FootballerAction
		pass teammate	= KickBall { vxy = { direction = bearing zero me teammate, velocity = ms 20.0 }, vz = ms 1.0 }
