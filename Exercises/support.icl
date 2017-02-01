implementation module support

import StdEnv, StdIO
	
toPoint2				:: Size Pos -> Point2
toPoint2 {w,h} {rx,ry}	= {x=toInt ((toReal w)*rx),y=toInt ((toReal h)*ry)}

fromPoint2				:: Size Point2 -> Pos
fromPoint2 {w,h} {x,y}	= {rx=(toReal x)/(toReal w),ry=(toReal y)/(toReal h)}

circle	 				:: Int -> Oval
circle r				= {oval_rx=r,oval_ry=r}

movePos					:: Vel Pos -> Pos
movePos v p				= {p & rx=p.rx+v.v0,ry=p.ry+v.v1}

class scale a			:: !Real !a -> a
class toVel a			:: !a       -> Vel
class dist a			:: !a !a    -> Real

instance zero Pos  where zero		= {rx=zero,ry=zero}
instance zero Vel  where zero		= {v0=zero,v1=zero}
instance +    Pos  where (+) p1 p2	= {rx=p1.rx+p2.rx,ry=p1.ry+p2.ry}
instance -    Pos  where (-) p1 p2	= {rx=p1.rx-p2.rx,ry=p1.ry-p2.ry}
instance +    Vel  where (+) a b	= {v0=a.v0+b.v0,v1=a.v1+b.v1}
instance -    Vel  where (-) a b	= {v0=a.v0-b.v0,v1=a.v1-b.v1}

instance scale Pos where scale r p	= {rx=r*p.rx,ry=r*p.ry}
instance scale Vel where scale r v	= {v0=r*v.v0,v1=r*v.v1}
instance toVel Pos where toVel p	= {v0=p.rx,v1=p.ry}

instance dist Pos  where dist p1 p2	= sqrt ((p1.rx - p2.rx)^2.0 + (p1.ry - p2.ry)^2.0)
instance dist Real where dist r1 r2	= abs (r1 - r2)
