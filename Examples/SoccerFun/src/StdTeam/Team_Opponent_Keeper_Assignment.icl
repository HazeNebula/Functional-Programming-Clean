implementation module Team_Opponent_Keeper_Assignment

import	Team
from	KeeperChallenger import keeperChallenger

Team_Opponent_Keeper :: !Home FootballField -> Team
Team_Opponent_Keeper home field
					= getFielders club home field
where
	club			= base_TeamName_Opponent_Keeper +++ if (home==West) "_W" "_E"

base_TeamName_Opponent_Keeper :: String
base_TeamName_Opponent_Keeper = "Opp_Keeper"

getFielders :: String Home FootballField -> [Footballer]
getFielders club home field
	# chal1			= {keeperChallenger {clubName=club,playerNr=2} & pos   = {px=west_edge + scale 0.025 field.flength,py=scale 0.20 field.fwidth}
						                                           , speed = {(keeperChallenger {clubName=club,playerNr=2}).speed & direction = rad (1.4*pi)}
					  }
	# chal2			= {keeperChallenger {clubName=club,playerNr=3} & pos   = {px=west_edge + scale 0.060 field.flength,py=scale 0.15 field.fwidth}}
	# chal3			= {keeperChallenger {clubName=club,playerNr=4} & pos   = {px=west_edge + scale 0.080 field.flength,py=scale 0.00 field.fwidth}
						                                           , speed = {(keeperChallenger {clubName=club,playerNr=4}).speed & direction = rad (0.0*pi)}
					  }
	# chal4			= {keeperChallenger {clubName=club,playerNr=5} & pos   = {px=west_edge + scale 0.060 field.flength,py=scale -0.15 field.fwidth}}
	# chal5			= {keeperChallenger {clubName=club,playerNr=6} & pos   = {px=west_edge + scale 0.025 field.flength,py=scale -0.20 field.fwidth}
						                                           , speed = {(keeperChallenger {clubName=club,playerNr=6}).speed & direction = rad (0.6*pi)}
					  }
	# fielders		= [chal1,chal2,chal3,chal4,chal5]
	# fielders		= map (mirrorDirection home) fielders
	| home == East	= fielders
	| otherwise		= mirror field fielders
where
	west_edge		= scale -0.5 field.flength
		
mirrorDirection :: !Home !Footballer -> Footballer
mirrorDirection home fielder
	= {fielder & speed = {fielder.speed & direction = if (d > rad pi) (d - rad pi) (d + rad pi)}}
where
	d				= fielder.speed.direction
