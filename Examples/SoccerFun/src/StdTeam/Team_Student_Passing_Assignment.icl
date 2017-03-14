implementation module Team_Student_Passing_Assignment

/**	Implement a solution to the passing assignment.

	Your team consists of six players, with player's numbers 2 upto 7.
	Below you should only change the definition of footballer to your solution.
	Do not change the positions, player identifications, nor noses. 
	Do not change the implementation of base_TeamName_Student_Passing.
*/

import Footballer

Team_Student_Passing :: !Home FootballField -> Team
Team_Student_Passing home field	= if (home == West) team (mirror field team)
where
	team						= [  {footballer {clubName=club,playerNr=nr} & pos = toPosition (scale (0.5*x) field.flength,scale (0.5*y) field.fwidth),nose = rad (dir*pi)}
								  \\ (x,y) <- positions
								   & nr    <- [2..]
								   & dir   <- noses
								  ]
	club						= base_TeamName_Student_Passing +++ if (home==West) "_W" "_E"
	positions					= [(-0.43, 0.00)
								  ,(-0.35, 0.30)
								  ,( 0.00,-0.10)
								  ,( 0.15, 0.20)
								  ,( 0.32, 0.10)
								  ,( 0.43,-0.05)
								  ]
	noses						= [1.8,0.0,1.5,0.5,1.2,0.2]
	footballer playerID			= defaultFootballer playerID		// implement your footballer here

base_TeamName_Student_Passing :: String
base_TeamName_Student_Passing = "Student Passing"
