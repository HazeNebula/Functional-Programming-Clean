implementation module Team_Student_Rounds_Assignment

/** Implement a solution to the rounds running assignment.

	Your team consists of one player.
	Below you only need to change the definition of footballer to your solution.
	Do not change the position and player identification.
	Do not change the implementation of base_TeamName_Student_Rounds.
*/
import Footballer

Team_Student_Rounds :: !Home !FootballField -> Team
Team_Student_Rounds home field		= if (home == West) team (mirror field team)
where
	team							= [  {footballer {clubName=club,playerNr=nr} & pos = toPosition (scale x field.flength,scale y field.fwidth)}
									  \\ (x,y) <- positions
									   & nr    <- [2..]
									  ]
	club							= base_TeamName_Student_Rounds +++ if (home==West) "_W" "_E"
	positions						= [(-0.49,0.00)]
	footballer playerID				= defaultFootballer playerID		// implement your footballer here

base_TeamName_Student_Rounds :: String
base_TeamName_Student_Rounds = "Student Rounds"
