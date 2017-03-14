implementation module Geometry

import StdEnvExt

:: Metre				=   Metre !Real

m						:: !Real -> Metre		// distance in metre
m metre					= Metre metre

instance zero           Metre                  where zero                          = Metre zero
instance +              Metre                  where + (Metre m1)  (Metre m2)      = Metre (m1 + m2)
instance -              Metre                  where - (Metre m1)  (Metre m2)      = Metre (m1 - m2)
instance scale          Metre                  where scale k       (Metre m)       = Metre (k * m)
instance abs            Metre                  where abs           (Metre m)       = Metre (abs m)
instance sign           Metre                  where sign          (Metre m)       = sign m
instance ~              Metre                  where ~             (Metre m)       = Metre (~m)
instance ==             Metre                  where == (Metre m1) (Metre m2)      = m1 == m2
instance <              Metre                  where <  (Metre m1) (Metre m2)      = m1 <  m2
instance toReal         Metre                  where toReal        (Metre m)       = m
instance toString       Metre                  where toString      (Metre m)       = m +++> " m."



::	Velocity			= MetrePerSecond !Real	// velocity in metre/second		

ms						:: !Real -> Velocity
ms v					= MetrePerSecond v

instance zero           Velocity               where zero                                       = MetrePerSecond zero
instance +              Velocity               where + (MetrePerSecond v1)  (MetrePerSecond v2) = MetrePerSecond (v1 + v2)
instance -              Velocity               where - (MetrePerSecond v1)  (MetrePerSecond v2) = MetrePerSecond (v1 - v2)
instance scale          Velocity               where scale k                (MetrePerSecond v)  = MetrePerSecond (k * v)
instance abs            Velocity               where abs                    (MetrePerSecond v)  = MetrePerSecond (abs v)
instance sign           Velocity               where sign                   (MetrePerSecond v)  = sign v
instance ~              Velocity               where ~                      (MetrePerSecond v)  = MetrePerSecond (~v)
instance ==             Velocity               where == (MetrePerSecond v1) (MetrePerSecond v2) = v1 == v2
instance <              Velocity               where <  (MetrePerSecond v1) (MetrePerSecond v2) = v1 <  v2
instance toReal         Velocity               where toReal                 (MetrePerSecond v)  = v
instance toString       Velocity               where toString               (MetrePerSecond v)  = v +++> "m/s"

instance zero           RVector                where zero                          = {dx = zero,             dy = zero}
instance +              RVector                where + v1 v2                       = {dx = v1.dx + v2.dx,    dy = v1.dy + v2.dy}
instance -              RVector                where - v1 v2                       = {dx = v1.dx - v2.dx,    dy = v1.dy - v2.dy}
instance one            RVector                where one                           = {dx = m 1.0,            dy = m 1.0}
instance scale          RVector                where scale k {dx, dy}              = {dx = scale k dx,       dy = scale k dy}
instance ~              RVector                where ~ v                           = zero - v
instance coords         RVector                where coords {dx,dy}                = [dx,dy]
instance zero           RVector3D              where zero                          = {dxy = zero,            dz = zero}
instance +              RVector3D              where + v1 v2                       = {dxy = v1.dxy + v2.dxy, dz = v1.dz + v2.dz}
instance -              RVector3D              where - v1 v2                       = {dxy = v1.dxy - v2.dxy, dz = v1.dz - v2.dz}
instance one            RVector3D              where one                           = {dxy = one,             dz = m 1.0}
instance scale          RVector3D              where scale k {dxy,dz}              = {dxy = scale k dxy,     dz = scale k dz}
instance ~              RVector3D              where ~ v                           = zero - v
instance coords         RVector3D              where coords {dxy={dx,dy},dz}       = [dx,dy,dz]


::	Angle				= Radian !Real

pi						:== 3.1415926535897932384

rad						:: !Real -> Angle
rad x					= Radian (normalize_radian x)

degree					:: !Int -> Angle
degree x				= rad (toReal x * pi / 180.0)

normalize_radian		:: !Real -> Real
normalize_radian r		= normalize r 100
where 
    normalize r 0		= abort "Loop in normalize_radian. Mail p.achten@cs.ru.nl"
	normalize r n
	| r < ~pi			= normalize (r + 2.0*pi) (n-1)
	| r >  pi			= normalize (r - 2.0*pi) (n-1)
	| otherwise			= r
  

instance zero           Angle                  where zero                          = Radian zero
instance +              Angle                  where + (Radian r1)  (Radian r2)    = Radian (normalize_radian (r1 + r2))
instance -              Angle                  where - (Radian r1)  (Radian r2)    = Radian (normalize_radian (r1 - r2))
instance scale          Angle                  where scale k        (Radian r)     = Radian (normalize_radian (k*r))
instance abs            Angle                  where abs            (Radian r)     = Radian (abs r)
instance sign           Angle                  where sign           (Radian r)     = sign r
instance ~              Angle                  where ~              (Radian r)     = Radian (~r)
instance ==             Angle                  where == (Radian r1) (Radian r2)    = r1 == r2
instance <              Angle                  where <  (Radian r1) (Radian r2)    = r1 <  r2
instance toReal         Angle                  where toReal         (Radian r)     = r
instance toInt          Angle                  where toInt          (Radian r)     = toInt (r * 180.0 / pi)
instance toString       Angle                  where toString       (Radian r)     = r +++> " rad"
instance toRVector      Angle                  where toRVector      (Radian r)     = {dx = Metre (cos r), dy = Metre (sin r)}
instance sinus          Angle                  where sinus          (Radian r)     = sin r
instance cosinus        Angle                  where cosinus        (Radian r)     = cos r
instance tangens        Angle                  where tangens        (Radian r)     = tan r
instance arcsinus       Angle                  where arcsinus       x              = Radian (asin x)
instance arccosinus     Angle                  where arccosinus     x              = Radian (acos x)
instance arctangens     Angle                  where arctangens     x              = Radian (atan x)

instance zero           Position               where zero                          = {px  = zero, py = zero}
instance ==             Position               where == p1  p2                     = p1.px == p2.px && p1.py == p2.py
instance coords         Position               where coords {px,py}                = [px,py]
instance toString       Position               where toString {px, py}			   = "{px=" +++ toString px +++ ",py=" +++ toString py +++ "}"
instance toRVector      Position               where toRVector p                   = {dx  = p.px, dy = p.py}
instance zero           Position3D             where zero                          = {pxy = zero, pz = zero}
instance ==             Position3D             where == p1  p2                     = p1.pxy == p2.pxy && p1.pz == p2.pz
instance coords         Position3D             where coords {pxy={px,py},pz}       = [px,py,pz]
instance toString       Position3D             where toString {pxy,pz}			   = "{pxy=" +++ toString pxy +++ ",pz=" +++ toString pz +++ "}"
	
instance toPosition     (!Metre,!Metre)        where toPosition     (x,y)          = {px=x,py=y}
instance toPosition     Position               where toPosition     p2D            = p2D
instance toPosition     Position3D             where toPosition     p3D            = p3D.pxy
instance fromPosition   (!Metre,!Metre)        where fromPosition   p2D            = (p2D.px,p2D.py)
instance fromPosition   Position               where fromPosition   p2D            = p2D
instance fromPosition   Position3D             where fromPosition   p2D            = {zero & pxy=p2D}
instance toPosition3D   (!Metre,!Metre,!Metre) where toPosition3D   (x,y,z)        = {pxy=toPosition (x,y),pz=z}
instance toPosition3D   Position               where toPosition3D   p2D            = {zero & pxy=p2D}
instance toPosition3D   Position3D             where toPosition3D   p3D            = p3D
instance fromPosition3D (!Metre,!Metre,!Metre) where fromPosition3D p3D            = (p3D.pxy.px,p3D.pxy.py,p3D.pz)
instance fromPosition3D Position               where fromPosition3D p3D            = p3D.pxy
instance fromPosition3D Position3D             where fromPosition3D p3D            = p3D

instance zero           Speed                  where zero                          = {direction=zero,velocity=zero}
instance ==             Speed                  where == sp1 sp2                    = sp1.direction == sp2.direction && sp1.velocity == sp2.velocity
instance toString       Speed                  where toString {direction,velocity} = "{direction=" +++ toString direction +++ ",velocity=" +++ toString velocity +++ "}"
instance toSpeed3D      Speed                  where toSpeed3D   s                 = {zero & vxy=s}
instance zero           Speed3D                where zero                          = {vxy=zero,vz=zero}
instance ==             Speed3D                where == sp1 sp2                    = sp1.vxy == sp2.vxy && sp1.vz == sp2.vz
instance toString       Speed3D                where toString {vxy,vz}			   = "{vxy=" +++ toString vxy +++ ",vz=" +++ toString vz +++ "}"
instance toSpeed        Speed3D                where toSpeed     s                 = s.vxy
instance fromSpeed      Speed3D                where fromSpeed   s                 = {zero & vxy=s}
instance fromSpeed3D    Speed                  where fromSpeed3D s                 = s.vxy

class    coords a				:: !a -> [Metre]

move_point						:: !RVector !Position -> Position
move_point {dx,dy} {px,py}		= {px=px+dx,py=py+dy}

move_point3D					:: !RVector3D !Position3D -> Position3D
move_point3D {dxy,dz} {pxy,pz}	= {pxy=move_point dxy pxy,pz=pz+dz}

repell							:: !Metre !Position !Position -> Position
repell minimum_distance base pos
| d == zero						= move_point {zero & dx=minimum_distance} pos
| d < minimum_distance			= move_point v` base //move_point (v` - v) pos
| otherwise						= pos
where
	d							= dist base pos
	v							= {dx = pos.px - base.px, dy = pos.py - base.py}
	v`							= scale ((toReal minimum_distance) / (toReal d)) v

attract							:: !Metre !Position !Position -> Position
attract maximum_distance base pos
| d > maximum_distance			= move_point v` base
| otherwise						= pos
where
	d							= dist base pos
	v							= {dx = pos.px - base.px, dy = pos.py - base.py}
	v`							= scale ((toReal maximum_distance) / (toReal d)) v

between_points					:: !(!Position,!Position) !Position -> Bool
between_points (a,b) c			= point_in_rectangle (a,b) c && (toReal dcx) / (toReal dcy) == (toReal dx) / (toReal dy)
where
	[min_x,max_x:_]				= sort [a.px,b.px]
	[min_y,max_y:_]				= sort [a.py,b.py]
	(dx, dy)					= (a.px - b.px, a.py - b.py)
	(dcx,dcy)					= (a.px - c.px, a.py - c.py)

point_in_rectangle				:: !(!Position,!Position) !Position -> Bool
point_in_rectangle (a,b) c		= isbetween c.px min_x max_x && isbetween c.py min_y max_y
where
	(min_x,max_x)				= minmax (a.px,b.px)
	(min_y,max_y)				= minmax (a.py,b.py)

point_to_rectangle				:: !(!Position,!Position) !Position -> Position
point_to_rectangle   (a,b) c
| point_in_rectangle (a,b) c	= c
| otherwise						= toPosition c`
where
	(min_x,max_x)				= minmax (a.px,b.px)
	(min_y,max_y)				= minmax (a.py,b.py)
	left						= c.px <= min_x
	right						= c.px >= max_x
	above						= c.py >= max_y
	below						= c.py <= min_y
	
	c` | left  && above			= (min_x,max_y)
	   | right && above			= (max_x,max_y)
	   | left  && below			= (min_x,min_y)
	   | right && below 		= (max_x,min_y)
	   | above					= (c.px, max_y)
	   | below					= (c.px, min_y)
	   | left					= (min_x,c.py )
	   | right					= (max_x,c.py )
	   | otherwise				= abort ("unsuspected error; please rotate with angles between pi and -pi\n")

size_vector						:: !RVector -> Metre
size_vector v					= Metre (dist` v z)
where z :: RVector; z = zero

size_vector3D					:: !RVector3D -> Metre
size_vector3D v					= Metre (dist` v z)
where z :: RVector3D; z = zero

dist							:: !a !b -> Metre | toPosition3D a & toPosition3D b
dist a b						= Metre (dist` (toPosition3D a) (toPosition3D b))

dist`							:: !a !b -> Real | coords a & coords b
dist` cs1 cs2					= sqrt (sum [ (toReal c1 - toReal c2)^2.0 \\ c1 <- coords cs1 & c2 <- coords cs2 ])

orthogonal						:: !Angle -> (!Angle,!Angle)
orthogonal a					= (a + rad (0.25*pi), a - rad (0.25*pi))

px_bearing						:: !base !target -> Angle | toPosition base & toPosition target
px_bearing base target
| abs ratio > 1.0				= Radian zero				// Corner-cases: d==0 and "large values of 1.0"
| v.dx >= zero && v.dy >= zero	= Radian base_angle			// 1st quadrant
| v.dx <= zero && v.dy >= zero	= Radian (pi-base_angle)	// 2nd quadrant
| v.dx <= zero && v.dy <= zero	= Radian (base_angle-pi)	// 3rd quadrant
| v.dx >= zero && v.dy <= zero	= Radian (~base_angle)		// 4th quadrant
where
	pbase						= toPosition base
	ptarget						= toPosition target
	v							= {dx = ptarget.px - pbase.px, dy = ptarget.py - pbase.py}
	d							= toReal (dist pbase ptarget)
    ratio						= toReal (abs v.dx) / d
	base_angle					= acos ratio

bearing							:: !Angle !base !target -> Angle | toPosition base & toPosition target
bearing angle base target		= px_bearing base target - angle
