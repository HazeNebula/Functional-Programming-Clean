definition module support

import StdEnv, StdIO

::	Pos							// A position
 =	{	rx		:: !Real		// 0.0 <= rx <= 1.0
 	,	ry		:: !Real		// 0.0 <= ry <= 1.0
 	}
::	Vel							// A velocity
 =	{	v0		:: !Real		// -1.0 <= v0 <= 1.0
 	,	v1		:: !Real		// -1.0 <= v1 <= 1.0
 	}

toPoint2		:: Size Pos -> Point2
fromPoint2		:: Size Point2 -> Pos

circle 			:: Int -> Oval

movePos			:: Vel Pos -> Pos

instance zero Pos
instance zero Vel
instance + Pos
instance - Pos
instance + Vel
instance - Vel

class scale a	:: !Real !a -> a
class toVel a	:: !a -> Vel
class dist  a	:: !a !a -> Real

instance scale Pos
instance scale Vel
instance toVel Pos
instance dist Pos
instance dist Real
