implementation module Football

import Footballer, randomstream

instance zero         Football where zero					= {ballPos=zero, ballSpeed=zero}
instance toPosition   Football where toPosition   {ballPos} = ballPos.pxy
instance toPosition3D Football where toPosition3D {ballPos}	= ballPos

mkFootball :: !Position !Speed -> Football
mkFootball pos2D speed2D			= {ballPos = toPosition3D pos2D, ballSpeed = toSpeed3D speed2D}

ballIsFree :: !FootballState -> Bool
ballIsFree (Free _)					= True
ballIsFree _						= False

ballIsGainedBy :: !FootballerID !FootballState -> Bool
ballIsGainedBy id (GainedBy id`)	= id == id`
ballIsGainedBy _ _					= False

getFootball :: !FootballState !.[Footballer] -> Football
getFootball (Free ball) _			= ball
getFootball (GainedBy playerID) allPlayers
	= case filter (identify_player playerID) allPlayers of
		[]							= abort "getFootball: no player found with requested identifier."
		[{pos,speed}:_]				= mkFootball pos speed
