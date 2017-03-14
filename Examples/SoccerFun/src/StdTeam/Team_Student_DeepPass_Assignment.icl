implementation module Team_Student_DeepPass_Assignment

/**	Implement a solution to the deep passing assignment.

	Your team consists of two players, with player's numbers 2 and 3 respectively.
	Below you should only change the definition of footballer to your solution.
	Do not change the positions and player identifications. 
	Do not change the implementation of base_TeamName_Student_DeepPass.
*/

import	Footballer

Team_Student_DeepPass :: !Home FootballField -> Team
Team_Student_DeepPass home field	= if (home == West) team (mirror field team)
where
	team							= [  {footballer {clubName=club,playerNr=nr} & pos = toPosition (scale (0.5*x) field.flength,scale (0.5*y) field.fwidth)}
									  \\ (x,y) <- positions
									   & nr    <- [2,3]
									  ]
	club							= base_TeamName_Student_DeepPass +++ if (home==West) "_W" "_E"
	positions						= [(0.05,-0.05),(0.35,0.05)]
	footballer playerID				= defaultFootballer playerID		// implement your footballer here

base_TeamName_Student_DeepPass :: String
base_TeamName_Student_DeepPass = "Student Deep Pass"
