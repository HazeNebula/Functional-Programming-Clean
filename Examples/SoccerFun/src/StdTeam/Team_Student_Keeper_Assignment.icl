implementation module Team_Student_Keeper_Assignment

/**	Implement a solution to the keeper assignment.

	Your team consists of a single keeper, with player's number 1.
	Below you should only change the definition of footballer to your solution.
	Do not change the positions and player identifications. 
	Do not change the implementation of base_TeamName_Student_Keeper.
*/

import	Footballer

Team_Student_Keeper				:: !Home FootballField -> Team
Team_Student_Keeper home field	= if (home == West) team (mirror field team)
where
	team						= [{footballer {clubName=club,playerNr=1} & pos = {zero & px = scale -0.485 field.flength}}]
	club						= base_TeamName_Student_Keeper +++ if (home == West) "_W" "_E"
	footballer playerID			= defaultFootballer playerID		// implement your footballer here

base_TeamName_Student_Keeper	:: String
base_TeamName_Student_Keeper	= "Student Keeper"

