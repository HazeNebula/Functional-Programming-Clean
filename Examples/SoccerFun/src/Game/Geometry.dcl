definition module Geometry

/**	This module defines the part of the SoccerFun API that is concerned with the footballer dimensions.
*/
import StdEnvExt


class    scale a		:: !Real !a -> a


:: Metre
m						:: !Real -> Metre			// (m metre) is a distance in metres

instance zero           Metre
instance +              Metre
instance -              Metre
instance abs            Metre
instance sign           Metre
instance ~              Metre
instance ==             Metre
instance <              Metre
instance toReal         Metre
instance toString       Metre
instance scale          Metre


::	Position
	=	{	px			:: !Metre					// x-coordinate in plane
		,	py			:: !Metre					// y-coordinate in plane
		}
::	Position3D
	=	{	pxy			:: !Position				// position on plane
		,	pz			:: !Metre					// height above plane
		}

instance zero           Position
instance zero           Position3D
instance ==             Position
instance ==             Position3D
instance toString       Position
instance toString       Position3D

class    toPosition     a :: !a -> Position
class    fromPosition   a :: !Position -> a
class    toPosition3D   a :: !a -> Position3D
class    fromPosition3D a :: !Position3D -> a
instance toPosition     (!Metre,!Metre)
instance toPosition     Position
instance toPosition     Position3D
instance fromPosition   (!Metre,!Metre)
instance fromPosition   Position
instance fromPosition   Position3D
instance toPosition3D   (!Metre,!Metre,!Metre)
instance toPosition3D   Position
instance toPosition3D   Position3D
instance fromPosition3D (!Metre,!Metre,!Metre)
instance fromPosition3D Position
instance fromPosition3D Position3D

dist					:: !a !b -> Metre | toPosition3D a & toPosition3D b


::	Angle

pi						:== 3.1415926535897932384	// (rad pi) is equivalant with (degree 180)

rad						:: !Real -> Angle			// (rad    x) is an angle of x radians (counter-clockwise)
degree					:: !Int  -> Angle			// (degree x) is an angle of x degrees (counter-clockwise)

instance zero           Angle
instance +              Angle
instance -              Angle
instance abs            Angle
instance sign           Angle
instance ~              Angle
instance ==             Angle
instance <              Angle
instance toReal         Angle
instance toInt          Angle
instance toString       Angle
instance scale          Angle
instance sinus          Angle
instance cosinus        Angle
instance tangens        Angle
instance arcsinus       Angle
instance arccosinus     Angle
instance arctangens     Angle

/** orthogonal a
		returns the left- and right- orthogonal angles to a
*/
orthogonal				:: !Angle -> (!Angle,!Angle)

/** bearing nose base target:
		returns the smallest rotation angle needed for a player at @base having current direction @nose
		to face @target.
		The result is a value between ~pi and pi.
*/
bearing					:: !Angle !base !target -> Angle | toPosition base & toPosition target


::	RVector
	=	{	dx			:: !Metre					// difference in px-coordinate
		,	dy			:: !Metre					// difference in py-coordinate
		}
::	RVector3D
	=	{	dxy			:: !RVector
		,	dz			:: !Metre
		}

class    toRVector a	:: !a -> RVector
instance toRVector      Angle
instance toRVector      Position

/** size_vector   {dx, dy} = sqrt (    dx^2 +     dy^2)
	size_vector3D {dxy,dz} = sqrt (dxy.dx^2 + dxy.dy^2 + dz^2)
*/
size_vector				:: !RVector   -> Metre
size_vector3D			:: !RVector3D -> Metre

instance +              RVector
instance +              RVector3D
instance -              RVector
instance -              RVector3D
instance zero           RVector
instance zero           RVector3D
instance one            RVector
instance one            RVector3D
instance ~              RVector
instance ~              RVector3D
instance scale          RVector
instance scale          RVector3D


/** move_point v p:
		moves point p over vector v.
	move_point3D v p:
		moves point p over vector v.
*/
move_point				:: !RVector   !Position   -> Position
move_point3D			:: !RVector3D !Position3D -> Position3D

/** repell minimum_distance base pos:
		if @pos is too close to @base, compute a new position that is at least @minimum_distance metres away from @base.
	attract maximum_distance base pos:
		if @pos is too far away from @base, compute a new position that is at most @maximum_distance metres away from @base.
*/
repell					:: !Metre !Position !Position -> Position
attract					:: !Metre !Position !Position -> Position

/**	point_to_rectangle (a,b) c
		returns c if (point_in_rectangle (a,b) c) and the projected point c` of c that is exactly on the closest edge of 
		rectangle (a,b).
*/
point_to_rectangle		:: !(!Position,!Position) !Position -> Position

/** point_in_rectangle (a,b) c
		returns True iff point c is inside the rectangle determined by 
		the diagonal corner points a and b.
*/
point_in_rectangle		:: !(!Position,!Position) !Position -> Bool


::	Velocity

ms						:: !Real -> Velocity		// (ms metre-per-second) is a velocity

instance zero           Velocity
instance +              Velocity
instance -              Velocity
instance abs            Velocity
instance sign           Velocity
instance ~              Velocity
instance ==             Velocity
instance <              Velocity
instance toReal         Velocity
instance toString       Velocity
instance scale          Velocity

		
::	Speed											// speed of an object
	=	{	direction	:: !Angle					// direction of object
		,	velocity	:: !Velocity				// velocity of object
		}
::	Speed3D											// speed of an object in space
	=	{	vxy			:: !Speed					// surface speed of object
		,	vz			:: !Velocity				// velocity in z-axis (positive: goes up; negative: goes down; zero: horizontally)
		}

instance zero           Speed
instance zero           Speed3D
instance ==             Speed
instance ==             Speed3D
instance toString       Speed
instance toString       Speed3D

class    toSpeed     a :: !a -> Speed
class    fromSpeed   a :: !Speed -> a
class    toSpeed3D   a :: !a -> Speed3D
class    fromSpeed3D a :: !Speed3D -> a

instance toSpeed        Speed3D
instance fromSpeed      Speed3D
instance toSpeed3D      Speed
instance fromSpeed3D    Speed
