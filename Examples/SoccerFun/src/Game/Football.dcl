definition module Football

/**	The Football data type and a number of access functions.
*/

import Footballer, randomstream

::	FootballState	= Free     !Football
					| GainedBy !FootballerID
::	Football		= { ballPos		:: !Position3D
					  , ballSpeed	:: !Speed3D
					  }
::	BounceDirection = Down | Up | Forward | Back

instance zero         Football
instance toPosition   Football
instance toPosition3D Football

/*	mkFootball returns a football with 3D dimensions.
*/
mkFootball			:: !Position !Speed -> Football

/*	ballIsFree yields True iff argument is (Free ...).
*/
ballIsFree			:: !FootballState -> Bool

/*	ballIsGainedBy yields True iff the ball is in possession by the given player.
*/
ballIsGainedBy		:: !FootballerID !FootballState -> Bool

/*	getFootball returns the football (containing its position and speed-information)
	that is either free or gained by a footballer.
	For this reason, the list of footballers must contain all footballers, otherwise
	this function may fail.
*/
getFootball			:: !FootballState !.[Footballer] -> Football

radius_football		:== m 0.113					// radius of football
surface_resistance	:== 0.85					// maximum speed of ball when moving over surface
air_resistance		:== 0.95					// maximum speed of ball when moving through air (should depend on velocity)
accelleration_sec	:== ms 9.81					// acceleration difference per square second
