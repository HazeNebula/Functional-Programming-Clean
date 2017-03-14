definition module RefereeCoach_Passing_Assignment

/** This module implements a referee coach for a ball passing assignment.
	The student footballer team should pass the ball from one end of the field to the other
	end of the field. Accepting players should not move further away than 10 metres from 
	their starting point. The last player should kick the ball in the goal to end the assignment.
	
	The opponent team should be Team_Opponent_Passing_Assignment.
	
	Developed by Wanja Krah.
*/

import Referee

RefereeCoach_Passing :: !FootballField -> Referee
