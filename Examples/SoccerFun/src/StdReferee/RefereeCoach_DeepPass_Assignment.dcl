definition module RefereeCoach_DeepPass_Assignment

/** This module implements a referee coach for a ball passing assignment.
	Two footballers from the student team are separated by a group of opponents who are crossing
	the field in north-south direction. The player at one side is in possession of the ball.
	He needs to pass the ball to the other player in such a way that it can not be gained by
	an opponent. 
	
	The opponent team should be Team_Opponent_DeepPass_Assignment.
	
	Developed by Wanja Krah.
*/

import Referee

RefereeCoach_DeepPass :: !FootballField -> Referee
