definition module RefereeCoach_Keeper_Assignment

/** This module implements a referee coach for a keeper assignment.
	The student team consists of a single keeper. He is challenged by TeamOpponent_Keeper_Assignment
	which consists of a number of footballers who are passing the ball to each other in an attempt
	to have the keeper not cover the center of the goal.
	
	Developed by Wanja Krah.
*/

import Referee

RefereeCoach_Keeper :: !FootballField -> Referee
