implementation module Team_Opponent_Passing_Assignment

import Footballer

Team_Opponent_Passing :: !Home FootballField -> Team
Team_Opponent_Passing home field
			= [defaultFootballer {clubName=club,playerNr=2}]
where
	club	= base_TeamName_Opponent_Passing +++ if (home == West) "_W" "_E"

base_TeamName_Opponent_Passing :: String
base_TeamName_Opponent_Passing = "Opp_Passing"
