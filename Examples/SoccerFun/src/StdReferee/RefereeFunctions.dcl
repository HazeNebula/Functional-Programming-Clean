definition module RefereeFunctions

/**	Functions for creating miniscule refereeing brains:
*/

import Referee

/** ball_left_field_at field:
	if the ball left the field, it returns (Just edge location) of the field where that happened;
	if the ball is inside the field, Nothing is returned.
*/
ball_left_field_at :: !FootballField -> RefereeAI` (Maybe Position)

/** ball_in_goal field:
	if the ball is in a goal, it returns the home of the field where the ball is;
	if the ball is not in a goal, Nothing is returned.
*/
ball_in_goal :: !FootballField -> RefereeAI` (Maybe Home)

/** half_of_game total_playing_time:
	returns which half of the game is currently being played, assuming that @total_playing_time
	is the correct time of the entire match.
*/
half_of_game :: !PlayingTime -> RefereeAI` Half

/** offside_players field home:
	returns the players from @home that are in offside position.
*/
players_in_offside_position :: !FootballField !Home -> RefereeAI` (AssocList FootballerID Position)
