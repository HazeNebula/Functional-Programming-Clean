implementation module Umpire

import RefereeFunctions

umpire						:: !FootballField -> Referee
umpire field				= { name			= "Umpire"
							  , brain			= {memory = Nothing, ai = brain field}
							  , refActionPics	= []
							  }

::	Memory					= { total_time		:: !PlayingTime							// the playing time
							  , current_half	:: !Half								// the current playing half (initially FirstHalf)
							  , placing1		:: !Displacements						// the placing of team1 at the start of the match
							  , placing2		:: !Displacements						// the placing of team2 at the start of the match
							  , forbidden		:: !Maybe Home							// players from (Just home) are not allowed to play the ball
							  , offside			:: ![FootballerID]						// players that are offside and thus should not play the ball
							  , reprimands		:: !AssocList FootballerID [Reprimand]	// the reprimands collected by each player
							  , situation		:: !Maybe (Situation,Pending)			// the situation of the game
							  }
::	Situation				= IsCenterKick | IsCornerKick | IsDirectFreeKick | IsGoalKick | IsPenaltyKick | IsThrowIn | IsKeeperBall !Home
::	Pending					= IsPending !Deadline | IsExecuted
::	Deadline			  :== Seconds

brain						:: !FootballField !(!RefereeInput,!(!Maybe Memory,!RandomSeed)) -> (!RefereeOutput,!(!Maybe Memory,!RandomSeed))
//	Referee enters the game, so (s)he needs to be filled in on the game details:
brain field (input=:{RefereeInput | team1,team2,playingTime},(Nothing,seed))
| team1_ok && team2_ok		= ([CenterKick West,DisplacePlayers ds], (Just memory,seed))
| otherwise					= ([TellMessage wrong_team,GameOver],    (Nothing,    seed))
where
	team1_ok				= isValidTeam team1 && allPlayersAtHome field West team1
	team2_ok				= isValidTeam team2 && allPlayersAtHome field East team2
	wrong_team				= if (not team1_ok) (nameOf team1 +++ " is invalid. ") "" +++
							  if (not team2_ok) (nameOf team2 +++ " is invalid. ") ""
	memory					= { total_time   = playingTime
							  , current_half = FirstHalf
							  , placing1     = displacements team1
							  , placing2     = displacements team2
							  , forbidden    = Just East
							  , offside      = []
							  , reprimands   = []
							  , situation    = Just (IsCenterKick,IsPending center_kick_deadline)
							  }
	ds						= center_kick_positions field West memory

//	Referee stops the game when a team has less than 7 players:
brain field (input=:{RefereeInput | team1, team2},(Just memory,seed))
| too_few_players			= ([GameCancelled winner,TellMessage ("Game cancelled." <+++ msg)],(Just memory,seed))
where
	too_few_players			= too_few_team1 || too_few_team2
	too_few_team1			= length team1 < 7
	too_few_team2			= length team2 < 7
	(winner,msg)			= if (too_few_team1 && too_few_team2) (Nothing,   "Both teams have less than 7 players left.")
							 (if  too_few_team1                   (Just East, nameOf team1 +++> " has less than 7 players left.")
							                                      (Just West, nameOf team2 +++> " has less than 7 players left.")
							 )

//	Referee checks whether the game is at the first half, second half, or completely over:
brain field (input=:{RefereeInput | playingTime},(Just memory=:{current_half,total_time},seed))
| playingTime <= zero		= ([GameOver],                                   (Just memory_no_offside,seed))
| current_half <> half		= ([EndHalf,CenterKick West,DisplacePlayers ds], (Just memory_2nd_half,  seed))
where
	half					= half_of_game total_time input
	memory_no_offside		= { memory            & situation = Nothing, offside = [] }
	memory_2nd_half			= { memory_no_offside & situation = Just (IsCenterKick,IsPending center_kick_deadline), current_half = half, forbidden = Just East }
	ds						= center_kick_positions field West memory_2nd_half

//	Referee checks whether a team has scored a goal:
brain field (input,(Just memory=:{current_half},seed))
| isJust in_goal			= ([Goal scoring_team,CenterKick (other scoring_team),DisplacePlayers ds],(Just memory_goal,seed))
where
	in_goal					= ball_in_goal field input
	goal					= fromJust in_goal
	scoring_team			= other goal
	ds						= center_kick_positions field goal memory
	memory_goal				= { memory & forbidden = Just scoring_team, offside = [], situation = Just (IsCenterKick,IsPending center_kick_deadline) }

//	Referee checks whether the keeper has caught the ball.
//	In that case the keeper is obliged to play the ball within 6 seconds.
brain field (input=:{RefereeInput | team1, team2},(Just memory=:{situation},seed))
| keeper_catches_ball		= ([],(Just memory_keeper_ball,seed))
where
	keeper_catches_ball		= not (isEmpty catchers) && catcher.playerNr == 1 && ball_was_uncaught
	ball_was_uncaught		= isNothing situation || isJust situation && fst (fromJust situation) <> IsKeeperBall team_of_catcher
	catchers				= [playerID \\ {playerID,effect=Just action} <- team1 ++ team2 | isCaughtBall action]
	catcher					= hd catchers
	team_of_catcher			= home_of_player catcher input
	memory_keeper_ball		= { memory & situation = Just (IsKeeperBall team_of_catcher,IsPending keeper_deadline)
							           , forbidden = Just (other team_of_catcher)
							           , offside   = []
							  }

//	Referee checks whether the ball has exited the football field.
//	If the last player who played the ball is not known, then the ball is thrown in by the team playing on the half of the field where the ball left the field.
brain field (input=:{RefereeInput | lastContact},(Just memory=:{current_half},seed))
| isJust ball_exit			=  if is_throw_in ([ThrowIn  (other team) exit_pos], (Just {memory_enter & situation = Just (IsThrowIn,   IsPending restart_deadline)},seed))
	    	                  (if is_corner   ([Corner   (other team) edge],     (Just {memory_enter & situation = Just (IsCornerKick,IsPending restart_deadline)},seed))
		                                      ([GoalKick (other team)],          (Just {memory_enter & situation = Just (IsGoalKick,  IsPending keeper_deadline )},seed))
		                      )
where
	ball_exit				= ball_left_field_at field input
	exit_pos				= fromJust ball_exit
	team					= case lastContact of
							     Just fID	= home_of_player fID input
							     unknown	= if (home == West) (if (current_half == FirstHalf) West East) (if (current_half == FirstHalf) East West)
	is_throw_in				= abs exit_pos.py >= scale 0.5 field.fwidth
	is_corner				= exit_pos.px <= scale -0.5 field.flength && team == West || exit_pos.px >= scale 0.5 field.flength && team == East
	home					= if (exit_pos.px < zero) West  East
	edge					= if (exit_pos.py < zero) North South
	memory_enter			= { memory & forbidden = Just team, offside = [] }

//	Referee checks whether the ball is not played correctly:
brain field (input=:{RefereeInput | team1, team2, playingHalf},(Just memory=:{forbidden},seed))
| improper_team				= ([OwnBallIllegally ball_player,DirectFreeKick (other team) player_pos,DisplacePlayers ds:map (ReprimandPlayer ball_player) new_reprimands]
							  ,(Just memory_illegal,seed)
							  )
where
	improper_team			= isJust forbidden && ball_is_played && team_of_ball_player == team
	ball_players			= [(playerID,pos) \\ {playerID,pos,effect=Just action} <- team1 ++ team2 | isBallAction action]
	ball_is_played			= not (isEmpty ball_players)
	(ball_player,player_pos)= hd ball_players
	team_of_ball_player		= home_of_player ball_player input
	team					= fromJust forbidden
	ds						= direct_free_kick_positions team player_pos input
	(new_reprimands,memory_reprimanded)
							= reprimand_player ball_player Warning { memory & offside = [], situation = Just (IsDirectFreeKick,IsPending free_kick_deadline) }
	expel					= isMember RedCard new_reprimands
	memory_illegal			= if expel (expel_player ball_player team_of_ball_player playingHalf memory_reprimanded) memory_reprimanded

//  Referee checks whether a player has been tackled:
brain field (input=:{RefereeInput | team1, team2, playingHalf},(Just memory=:{current_half},seed))
| player_tackled			= ([TackleDetected offender,DirectFreeKick (other team) (snd victim),DisplacePlayers ds:map (ReprimandPlayer offender) new_reprimands]
							  ,(Just memory_tackle,seed)
							  )
where
	player_tackled			= not (isEmpty (players_down1 ++ players_down2)) && not (isEmpty (near_victims1 ++ near_victims2))
	players_down1			= [(playerID,pos) \\ {playerID,pos,effect=Just (OnTheGround frames)} <- team1 | frames > 0]
	near_victims1			= [playerID \\ {playerID,pos} <- team2 | isNear pos (map snd players_down1)]
	players_down2			= [(playerID,pos) \\ {playerID,pos,effect=Just (OnTheGround frames)} <- team2 | frames > 0]
	near_victims2			= [playerID \\ {playerID,pos} <- team1 | isNear pos (map snd players_down2)]
	offender				= hd (near_victims1 ++ near_victims2)
	victim					= hd (players_down1 ++ players_down2)
	team					= home_of_player offender input
	ds						= direct_free_kick_positions team (snd victim) input
	(new_reprimands,memory_reprimanded)
							= reprimand_player offender YellowCard { memory & offside = [], forbidden = Just team, situation = Just (IsDirectFreeKick,IsPending free_kick_deadline) }
	expel					= isMember RedCard new_reprimands
	memory_tackle			= if expel (expel_player offender team playingHalf memory_reprimanded) memory_reprimanded
	
	isNear p1 ps			= or [dist p1 p <= crime_scene_distance \\ p <- ps]

//	Referee checks whether the hands-rule has been offended:
brain field (input=:{RefereeInput | team1, team2, playingHalf},(Just memory=:{current_half},seed))
| hands_offense				= ([Hands catcher,DirectFreeKick (other team) catcher_pos,DisplacePlayers ds:map (ReprimandPlayer catcher) new_reprimands]
							  ,(Just memory_hands,seed)
							  )
where
	hands_offense			= ball_is_caught && (catcher.playerNr <> 1 || not (inPenaltyArea field team catcher_pos))
	catchers				= [(playerID,pos) \\ {playerID,pos,effect=Just action} <- team1 ++ team2 | isCaughtBall action]
	(catcher,catcher_pos)	= hd catchers
	ball_is_caught			= not (isEmpty catchers)
	team					= home_of_player catcher input
	ds						= direct_free_kick_positions team catcher_pos input
	(new_reprimands,memory_reprimanded)
							= reprimand_player catcher YellowCard { memory & forbidden = Just team, offside = [], situation = Just (IsDirectFreeKick,IsPending free_kick_deadline) }
	expel					= isMember RedCard new_reprimands
	memory_hands			= if expel (expel_player catcher team playingHalf memory_reprimanded) memory_reprimanded

//	Referee checks whether the offside-rule has been offended:
brain field (input=:{RefereeInput | theBall, team1, team2},(Just memory=:{offside},seed))
| offside_offense			= ([Offside offender.playerID,DirectFreeKick (other team) offender.pos,DisplacePlayers ds],(Just memory_offside_lifted,seed))	// this should really be an indirect free kick, but that is not implemented yet
where
	offside_offense			= not (isEmpty offenders)
	offenders				= [player \\ playerID <- offside										// offside is activated by a player in offside position
							           , player   <- filter (identify_player playerID) players 
							           | dist ball player < maxKickReach player						// who can play the ball (is actively engaged)
							  ]
	offender				= hd offenders
	players					= team1 ++ team2
	ball					= getFootball theBall players
	team					= home_of_player offender.playerID input
	ds						= direct_free_kick_positions team offender.pos input
	memory_offside_lifted	= { memory & forbidden = Just team, offside = [], situation = Just (IsDirectFreeKick,IsPending free_kick_deadline) }

//	Referee checks whether a team is passive:
brain field (input=:{RefereeInput | theBall, team1, team2, playingHalf},(Just memory=:{forbidden=Just team,situation=Just (state,IsPending dt)},seed))
| passivity					= ([TellMessage msg,DirectFreeKick team ball.ballPos.pxy,DisplacePlayers ds],(Just memory_passive,seed))
where
	passivity				= dt < zero
	memory_passive			= { memory & forbidden = Just (other team), offside = [], situation = Just (IsDirectFreeKick,IsPending free_kick_deadline)}
	ball					= getFootball theBall (team1 ++ team2)
	ds						= direct_free_kick_positions (other team) ball.ballPos.pxy input
	msg						= "Passive play by " <+++ nameOf (if (team == West && playingHalf == FirstHalf || team == East && playingHalf == SecondHalf) team2 team1)

//	Referee checks the status of the rules and remains silent to let the game continue:
brain field (input=:{RefereeInput | theBall, team1, team2},(Just memory=:{situation,forbidden},seed))
# memory					= decrease_pending_time input memory
# memory					= if  ball_is_played                              { memory & situation = new_situation } memory
# memory					= if (ball_is_played && not no_offside_situation) { memory & offside   = at_offside    } memory
# memory					= if (ball_is_played && lift_forbidden_rule)      { memory & forbidden = Nothing       } memory
= ([],(Just memory,seed))
where
	new_situation			= if (isJust situation) (case pending of
							                            IsPending _ = Just (state,IsExecuted)
							                            _           = Nothing
							                        ) Nothing
	no_offside_situation	= isJust situation && isMember state [IsCornerKick,IsGoalKick,IsThrowIn]
	lift_forbidden_rule		= isJust forbidden && team_of_ball_player == other forbidden_team
	(state,pending)			= fromJust situation
	forbidden_team			= fromJust forbidden
	ball_players			= [playerID \\ {playerID,effect=Just action} <- team1 ++ team2 | isBallAction action]
	ball_is_played			= not (isEmpty ball_players)
	ball_player				= hd ball_players
	team_of_ball_player		= home_of_player ball_player input
	at_offside				= [playerID \\ (playerID,_) <- players_in_offside_position field team_of_ball_player input | playerID <> ball_player]

decrease_pending_time		:: !RefereeInput !Memory -> Memory
decrease_pending_time input=:{RefereeInput | unittime} memory=:{situation = Just (state,IsPending dt)}
							= { memory & situation = Just (state,IsPending (dt - unittime)) }
decrease_pending_time _ memory
							= memory

expel_player				:: !FootballerID !Home !Half !Memory -> Memory
expel_player player team half memory=:{placing1,placing2}
| team == West && half == FirstHalf || team == East && half == SecondHalf
							= { memory & placing1 = deletekeyvalue player placing1 }
| otherwise					= { memory & placing2 = deletekeyvalue player placing2 }

reprimand_player			:: !FootballerID !Reprimand !Memory -> (![Reprimand],!Memory)
reprimand_player player reprimand memory=:{reprimands}
# new_reprimands			= [reprimand]
# new_reprimands			= if (length (filter ((==) Warning)    (new_reprimands ++ player_reprimands)) >= 3) (new_reprimands ++ [YellowCard]) new_reprimands
# new_reprimands			= if (length (filter ((==) YellowCard) (new_reprimands ++ player_reprimands)) >= 2) (new_reprimands ++ [RedCard])    new_reprimands
= (new_reprimands, {memory & reprimands = addkeyvalue (player,player_reprimands ++ new_reprimands) reprimands})
where
	player_reprimands		= lookupd [] player reprimands

home_of_player				:: !FootballerID !RefereeInput -> Home
home_of_player player {RefereeInput | playingHalf,team1}
| sameClub player (hd team1).playerID
							= if (playingHalf == FirstHalf) West East
| otherwise					= if (playingHalf == FirstHalf) East West

isPlayBallAction			:: !FootballerEffect -> Bool
isPlayBallAction action		= isKickedBall action || isHeadedBall action

isBallAction				:: !FootballerEffect -> Bool
isBallAction action			= isKickedBall action || isHeadedBall action || isGainedBall action || isCaughtBall action

center_kick_positions		:: !FootballField !Home !Memory -> Displacements
center_kick_positions field home_kicking_off {placing1,placing2,current_half}
| home_kicking_off == West	= kick_off positions1 ++ map repell_from_center positions2
| otherwise					= map repell_from_center positions1 ++ kick_off positions2
where
	(positions1,positions2)	= if (current_half == FirstHalf) (placing1,placing2)
							                                 ([(playerID,mirror field pos) \\ (playerID,pos) <- placing2]
							                                 ,[(playerID,mirror field pos) \\ (playerID,pos) <- placing1]
							                                 )
	center					= zero
	repell_from_center		= \(player,pos) -> (player,repell  radius_centre_circle center pos)
	attract_to_center		= \(player,pos) -> (player,attract (m 0.5) center pos)
	kick_off placement		= map attract_to_center closest ++ map repell_from_center others
	where
		sorted				= sortBy (\(_,pos1) (_,pos2) -> dist pos1 center < dist pos2 center) placement
		(closest,others)	= splitAt 2 sorted

/**	direct_free_kick_positions home pos input:
		move players of @home away from @pos, and attract the closest fielder of (other @home) to @pos.
*/
direct_free_kick_positions	:: !Home !Position !RefereeInput -> Displacements
direct_free_kick_positions team free_kick_pos input=:{RefereeInput | team1, team2, playingHalf}
	= [attract_kicker : push_away_offenders]
where
	(offenders,free_kickers)= if (team == West && playingHalf == FirstHalf || team == East && playingHalf == SecondHalf) (team1,team2) (team2,team1)
	push_away_offenders		= map (\{playerID,pos} -> (playerID,repell repell_distance free_kick_pos pos)) offenders
	closest_player			= snd (hd (sortBy (\(d1,_) (d2,_) -> d1 < d2) [(dist free_kick_pos player,player) \\ player <- free_kickers | isFielder player]))
	attract_kicker			= (closest_player.playerID,attract (m 1.0) free_kick_pos closest_player.pos)

keeper_deadline				:== s  6.0
center_kick_deadline		:== s  1.0
free_kick_deadline			:== s  1.0
restart_deadline			:== s 20.0
crime_scene_distance		:== m  1.0

instance ==       Situation	where == IsCenterKick      IsCenterKick      = True
							      == IsCornerKick      IsCornerKick      = True
							      == IsDirectFreeKick  IsDirectFreeKick  = True
							      == IsGoalKick        IsGoalKick        = True
							      == IsPenaltyKick     IsPenaltyKick     = True
							      == IsThrowIn         IsThrowIn         = True
							      == (IsKeeperBall t1) (IsKeeperBall t2) = t1 == t2
							      == _                 _                 = False
instance ==       Pending	where == (IsPending t1)    (IsPending t2)    = t1 == t2
							      == IsExecuted        IsExecuted        = True
							      == _                 _                 = False
instance toString Situation	where toString IsCenterKick                  = "IsCenterKick"
							      toString IsCornerKick                  = "IsCornerKick"
							      toString IsDirectFreeKick              = "IsDirectFreeKick"
							      toString IsGoalKick                    = "IsGoalKick"
							      toString IsPenaltyKick                 = "IsPenaltyKick"
							      toString IsThrowIn                     = "IsThrowIn"
							      toString (IsKeeperBall h)              = "(IsKeeperBall " <+++ h <+++ ")"
instance toString Pending	where toString (IsPending t)                 = "(IsPending "    <+++ t <+++ ")"
							      toString IsExecuted                    = "IsExecuted"
