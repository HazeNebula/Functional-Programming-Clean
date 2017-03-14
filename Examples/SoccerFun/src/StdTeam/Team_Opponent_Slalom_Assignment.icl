implementation module Team_Opponent_Slalom_Assignment

import StdReal
import Footballer

Team_Opponent_Slalom :: !Home FootballField -> Team
Team_Opponent_Slalom home field
				= if (home == West) dummies (mirror field dummies)
where
	club		= base_TeamName_Opponent_Slalom +++ if (home == West) "_W" "_E"
	keeper		= Nothing
	dummies		= [dummy1,dummy2,dummy3,dummy4,dummy5]
	dummy1		= {defaultFootballer {clubName=club,playerNr=2} & pos={zero & px = west_edge + scale 0.09 field.flength}}
	dummy2		= {defaultFootballer {clubName=club,playerNr=3} & pos={zero & px = west_edge + scale 0.19 field.flength}}
	dummy3		= {defaultFootballer {clubName=club,playerNr=4} & pos={zero & px = west_edge + scale 0.24 field.flength}}
	dummy4		= {defaultFootballer {clubName=club,playerNr=5} & pos={zero & px =             scale 0.11 field.flength}}
	dummy5		= {defaultFootballer {clubName=club,playerNr=6} & pos={zero & px =             scale 0.15 field.flength}}
	west_edge	= scale -0.5 field.flength

base_TeamName_Opponent_Slalom :: String
base_TeamName_Opponent_Slalom = "Opp_Slalom"
