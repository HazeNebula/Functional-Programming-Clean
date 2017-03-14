implementation module RefereeFunctions

import Referee

ball_left_field_at :: !FootballField -> RefereeAI` (Maybe Position)
ball_left_field_at field=:{fwidth,flength}
	= \{RefereeInput | theBall}
		-> case theBall of
		      Free ball	= if (point_in_rectangle field_coordinates ball.ballPos.pxy)
		                     Nothing
		                     (Just (point_to_rectangle field_coordinates ball.ballPos.pxy))
		      gained	= Nothing		// footballers are incapable of leaving the football field
where
	field_coordinates	= ({px = scale -0.5 flength, py = scale -0.5 fwidth}, {px = scale 0.5 flength, py = scale 0.5 fwidth})

ball_in_goal :: !FootballField -> RefereeAI` (Maybe Home)
ball_in_goal field=:{flength}
	= \{RefereeInput | theBall}
		-> case theBall of
		      Free ball = let ball_inside	= isbetween ball.ballPos.pxy.py south north && ball.ballPos.pz <= goal_height
		                   in if (ball.ballPos.pxy.px < scale -0.5 flength && ball_inside) (Just West)
		                     (if (ball.ballPos.pxy.px > scale  0.5 flength && ball_inside) (Just East)
		                                                                                   Nothing
		                     )
		      gained    = Nothing		// footballers must kick the ball in the goal to score
where
	(north,south)		= goal_poles field

half_of_game :: !PlayingTime -> RefereeAI` Half
half_of_game total_time
	= \{RefereeInput | playingTime}
		-> if (playingTime >= scale 0.5 total_time) FirstHalf SecondHalf

players_in_offside_position :: !FootballField !Home -> RefereeAI` (AssocList FootballerID Position)
players_in_offside_position field home
	= \{RefereeInput | theBall,playingHalf,team1,team2}
		-> let (players1,players2)	= if (home == West && playingHalf == FirstHalf || home == East && playingHalf == SecondHalf)
		                                 (displacements team1, displacements team2)
		                                 (displacements team2, displacements team1)
		       ball					= getFootball theBall (team1 ++ team2)
		    in offside_players field ball home players1 players2

/** offside_players field ball home team opponents:
	returns the players from @team that are in offside position.
	@home is the current home of @team, and @opponents are the current opponents.
	@ball is the current ball.
*/
offside_players :: !FootballField !Football !Home !Displacements !Displacements -> Displacements
offside_players field ball home team opponents
	= [(player,pos) \\  (player,pos) <- team             // a player is in offside position if:
	                 |  team_ord pos.px middle_x         // he is at the opponent's half of the field, and
	                 && team_ord pos.px opponent_x       // he is closer to the opponent's base line than the 2nd-last opponent, and
	                 && team_ord pos.px ball_x           // he is closer to the opponent's base line than the ball
	  ]
where
	middle_x		= zero
	ball_x			= ball.ballPos.pxy.px
	team_ord		= if (home == West) (>) (<)
	opponent_x		= (snd ((sortBy opponent_ord opponents) !! 1)).px
	opponent_ord	= if (home == West) (\(_,p1) (_,p2) -> p1.px >= p2.px) (\(_,p1) (_,p2) -> p1.px <= p2.px)
