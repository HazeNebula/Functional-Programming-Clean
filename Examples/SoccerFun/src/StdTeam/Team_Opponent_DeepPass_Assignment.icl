implementation module Team_Opponent_DeepPass_Assignment

import StdEnvExt
import Team
from   Buffer import buffer

Team_Opponent_DeepPass :: !Home FootballField -> Team
Team_Opponent_DeepPass home field
					= map tagName fielders
where
	club			= base_TeamName_Opponent_DeepPass +++ if (home == West) "_W" "_E"
	fielders		= getFielders club home field

base_TeamName_Opponent_DeepPass :: String
base_TeamName_Opponent_DeepPass = "Opp_Deep_Pass"

getFielders :: String Home FootballField -> [Footballer]
getFielders club home field
	# buf1			= {buffer field home south {clubName=club,playerNr=2 } & pos={px=scale 0.07 field.flength, py=scale  0.40 field.fwidth}}
	# buf2			= {buffer field home south {clubName=club,playerNr=3 } & pos={px=scale 0.12 field.flength, py=scale  0.08 field.fwidth}}
	# buf3			= {buffer field home north {clubName=club,playerNr=4 } & pos={px=scale 0.06 field.flength, py=scale  0.00 field.fwidth}}
	# buf4			= {buffer field home south {clubName=club,playerNr=5 } & pos={px=scale 0.11 field.flength, py=scale -0.20 field.fwidth}}
	# buf5			= {buffer field home north {clubName=club,playerNr=6 } & pos={px=scale 0.12 field.flength, py=scale  0.20 field.fwidth}}
	# buf6			= {buffer field home south {clubName=club,playerNr=7 } & pos={px=scale 0.13 field.flength, py=scale  0.00 field.fwidth}}
	# buf7			= {buffer field home south {clubName=club,playerNr=8 } & pos={px=scale 0.10 field.flength, py=scale -0.19 field.fwidth}}
	# buf8			= {buffer field home north {clubName=club,playerNr=9 } & pos={px=scale 0.15 field.flength, py=scale  0.07 field.fwidth}}
	# buf9			= {buffer field home north {clubName=club,playerNr=10} & pos={px=scale 0.05 field.flength, py=scale -0.08 field.fwidth}}
	# buf10			= {buffer field home south {clubName=club,playerNr=11} & pos={px=scale 0.15 field.flength, py=scale  0.22 field.fwidth}}
	# buf11			= {buffer field home north {clubName=club,playerNr=12} & pos={px=scale 0.08 field.flength, py=scale -0.14 field.fwidth}}
	# fielders		= [buf1,buf2,buf3,buf4,buf5,buf6,buf7,buf8,buf9,buf10,buf11]
	| home == East	= fielders
	| otherwise		= mirror field fielders
where
	(south,north)	= if (home == West) (North,South) (South,North)
		
tagName :: !Footballer -> Footballer
tagName fb=:{playerID,name}	= {Footballer | fb & name = name <+++ "_" <+++ playerID.playerNr}
