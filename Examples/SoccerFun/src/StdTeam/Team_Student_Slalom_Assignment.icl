implementation module Team_Student_Slalom_Assignment

/**	Implement a solution to the slalom assignment.

	Your team consists of one field player, with player's number 2.
	Below you should only change the definition of footballer to your solution.
	Do not change the position and player identification. 
	Do not change the implementation of base_TeamName_Student_Slalom.
*/

import Footballer
import FootballerFunctions

Team_Student_Slalom :: !Home FootballField -> Team
Team_Student_Slalom home field	= team
where
	team						= [{footballer {clubName=club,playerNr=2} & pos = if (home == West) position (mirror field position)}]
	club						= base_TeamName_Student_Slalom +++ if (home == West) "_W" "_E"
	position					= {zero & px = scale -0.5 field.flength + penalty_area_depth}
	footballer player_id		= defaultFootballer player_id	// implement your footballer here

base_TeamName_Student_Slalom :: String
base_TeamName_Student_Slalom	= "Student Slalom"
