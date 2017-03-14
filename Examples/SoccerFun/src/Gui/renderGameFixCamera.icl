implementation module renderGameFixCamera

import Geometry
import render

renderFixCamera						:: RenderStyle
renderFixCamera						= { name = "Fixed camera"
									  , look = fixcamera
									  }

fixcamera							:: !Match !SelectState !UpdateState !*Picture -> *Picture
fixcamera match=:{theField,theBall,team1,team2} _ updSt=:{newFrame} picture
# picture							= setPenColour Black picture
# picture							= fill newFrame      picture								// erase entire background
# picture       	        		= setPenColour green picture
# picture							= drawField          picture								// draw the field and outer lines
# picture							= drawMiddleArea     picture								// middle line, circle and spot
# picture							= drawGoalAreas      picture								// goal areas
# picture							= drawPenaltyAreas   picture								// penalty areas
# picture							= drawCornerAreas    picture								// corner areas
# picture							= drawGoals          picture								// goals
# picture							= foldr drawPlayer   picture sorted_players					// players
# picture							= drawBall (getFootball theBall (team1 ++ team2)) picture	// football
= picture
where
	{w,h}							= rectangleSize newFrame
	{fwidth,flength}				= theField
	ratio							= min ((toReal w) / (toReal flength)) ((toReal h) / (toReal fwidth))
	pix x							= toInt (ratio * toReal x)									// convert metres   to pixels
	ppix {px,py}					= {x = pix px, y = pix py}									// convert Position to pixels
	(northPole,southPole)			= goal_poles theField
	fieldfit	                	= min (toReal w / (ratio * (convertToIsoXNormal (toReal flength) 0.0             - convertToIsoXNormal 0.0 (toReal fwidth)))) 
									      (toReal h / (ratio * (convertToIsoYNormal (toReal flength) (toReal fwidth) - convertToIsoYNormal 0.0 0.0)))
	north_west 						= ppix (convertToIso {px = zero,    py = zero  })
	north_east 						= ppix (convertToIso {px = flength, py = zero  })
	south_east 						= ppix (convertToIso {px = flength, py = fwidth})
	south_west	 					= ppix (convertToIso {px = zero,    py = fwidth})
	sorted_players					= let se = {px=scale 0.5 flength, py=scale -0.5 fwidth}
									   in sortBy (\(_,fb1) (_,fb2) -> dist fb1 se < dist fb2 se) ([(WestColour,fb) \\ fb <- team1] ++ [(EastColour,fb) \\ fb <- team2])
	
	drawPlayer						:: !(!Colour, !Footballer) !*Picture -> *Picture
	drawPlayer (colour, fb=:{playerID ,name,nose=nose_dir,effect,pos=pos`}) picture
	# picture						= drawName name picture
	| perhaps isOnTheGround effect	= drawGroundPlayer   fb picture
	| otherwise						= drawStandingPlayer fb picture
	where
		pos							= move_point {dx=scale 0.5 flength, dy=scale 0.5 fwidth} {pos` & py = ~pos`.py}
		player_width				= m 0.5
		body_circle					= circle (pix player_width)
		bearing_circle				= circle (pix (m 0.3))
		nose_dir_point k			= move_point (scale k (convertToIsoVector {dx = m (cosinus nose_dir), dy = m (~(sinus nose_dir))})) (convertToIso pos)
		
		drawName name picture
		# picture					= setPenColour White                                     picture
		# picture					= drawAt (ppix (move_point one (convertToIso pos))) name picture
		= picture
		
		drawGroundPlayer fb=:{length} picture
		# (w,picture)				= getPenSize                                                          picture
		# picture					= setPenSize (pix player_width + 2)                                   picture
		# picture					= setPenColour Black                                                  picture
		# picture					= draw (body_line offset)                                             picture
		# picture					= setPenSize (pix player_width)                                       picture
		# picture					= setPenColour colour                                                 picture
		# picture					= draw (body_line zero)                                               picture
		# picture					= setPenSize w                                                        picture
		# picture					= fillAt (ppix (nose_dir_point (toReal length + 0.2))) bearing_circle picture
		# picture					= setPenColour Black                                                  picture
		# picture					= drawAt (ppix (nose_dir_point (toReal length + 0.2))) bearing_circle picture
		= picture
		where
			offset					= {vx = -1, vy = -1}
			body_line v				= { line_end1 = movePoint v (ppix (convertToIso pos))
									  , line_end2 = movePoint v (ppix (nose_dir_point (toReal length))) 
									  }
		
		drawStandingPlayer fb=:{length} picture
		# picture					= setPenColour colour                               picture
		# picture					= fillAt (ppix (nose_dir_point 1.0)) bearing_circle picture					// nose direction
		# picture					= foldr (uncurry fillAt)                            picture playerCorpus
		# picture					= setPenColour Black                                picture
		# picture					= drawAt (ppix (nose_dir_point 1.0)) bearing_circle picture					// surrounding circle
		# picture					= foldr (uncurry drawAt)                            picture playerCorpus
		= picture
		where
			player_width`			= toReal player_width
			length`					= pix length
			(up,down)				= ({vx = 0, vy = ~length`}, {vx = 0,vy = length`})
			playerCorpus			= [(ptl,{polygon_shape = [up,{vx = ptr.x-ptl.x, vy = ptr.y-ptl.y},down]})
									  ,(pbl,{polygon_shape = [up,{vx = pbr.x-pbl.x, vy = pbr.y-pbl.y},down]})
									  ,(pbr,{polygon_shape = [up,{vx = ptr.x-pbr.x, vy = ptr.y-pbr.y},down]})
									  ,(pbl,{polygon_shape = [up,{vx = ptl.x-pbl.x, vy = ptl.y-pbl.y},down]})
									  ]
			playerRadius			= m (sqrt (player_width` * player_width` + player_width` * player_width`))
			add_angle a				= let c = cosinus (a + nose_dir)
									      s =   sinus (a + nose_dir)
									   in ppix (convertToIso (move_point {dx = scale c playerRadius, dy = scale s playerRadius} pos))
			ptr						= add_angle (rad (0.25*pi))
			ptl						= add_angle (rad (0.75*pi))
			pbl						= add_angle (rad (1.25*pi))
			pbr						= add_angle (rad (1.75*pi))
			
	drawMiddleArea					:: !*Picture -> *Picture
	drawMiddleArea picture
	# picture						= drawLine {x = (north_west.x+north_east.x)/2, y = (north_west.y+north_east.y)/2} 
									           {x = (south_west.x+south_east.x)/2, y = (south_west.y+south_east.y)/2} picture		// middle line
	# picture						= drawAt center (circle (pix (scale fieldfit radius_centre_spot)))                picture		// centre spot
	# picture						= drawAt centerRadius {polygon_shape = circle_vs}                                 picture		// centre circle
	= picture
	where
		center						= {x = sum [north_west.x,north_east.x,south_west.x,south_east.x] / 4
									  ,y = sum [north_west.y,north_east.y,south_west.y,south_east.y] / 4
									  }
		centerRadius				= ppix (convertToIso {px = scale 0.5 flength, py = scale 0.5 fwidth - radius_centre_circle})	// centre circle starting location for drawing
		start						= {px=radius_centre_circle, py=zero}															// starting location for generating circle points
		circle_vs					= circle_shape radius_centre_circle (rad (0.025*pi)) (rad (2.0*pi)) start zero []
		
	drawGoals						:: !*Picture -> *Picture
	drawGoals picture
	# picture						= drawLine (ppix                ne)  (ppix (addGoalHeight ne)) picture	// east north pole
	# picture						= drawLine (ppix                se)  (ppix (addGoalHeight se)) picture	// east south pole
	# picture 						= drawLine (ppix (addGoalHeight ne)) (ppix (addGoalHeight se)) picture	// east pole connect
	# picture						= drawLine (ppix                nw)  (ppix (addGoalHeight nw)) picture	// west north pole
	# picture						= drawLine (ppix                sw)  (ppix (addGoalHeight sw)) picture	// west south pole
	# picture	 					= drawLine (ppix (addGoalHeight nw)) (ppix (addGoalHeight sw)) picture	// west pole connect
	= picture
	where 
		addGoalHeight p				= move_point   {zero & dy = ~goal_height} p
		nw							= convertToIso {px = zero,    py = northPole + scale 0.5 fwidth}
		sw							= convertToIso {px = zero,    py = southPole + scale 0.5 fwidth}
		ne							= convertToIso {px = flength, py = northPole + scale 0.5 fwidth}
		se							= convertToIso {px = flength, py = southPole + scale 0.5 fwidth}
		
	drawField						:: !*Picture -> *Picture
	drawField picture
	# picture						= fillAt north_west field_shape picture //field
	# picture 						= setPenColour White            picture
	# picture						= drawAt north_west field_shape picture //outline
	= picture
	where
		field_shape					= {polygon_shape = [{vx = north_east.x-north_west.x, vy = north_east.y-north_west.y}
									                   ,{vx = south_east.x-north_east.x, vy = south_east.y-north_east.y}
									                   ,{vx = south_west.x-south_east.x, vy = south_west.y-south_east.y}
									                   ]
									  }
		
	drawBall						:: !Football !*Picture -> *Picture
	drawBall football picture
	# picture						= setPenColour ball_colour        picture
	# picture						= drawAt (ppix ballPosition) ball picture	// ball shadow
	# picture						= fillAt (ppix elevation)    ball picture	// ball
	# picture						= setPenColour White              picture
	# picture						= drawAt (ppix elevation)    ball picture	// ball-line
	= picture
	where
		groundPos`					= football.ballPos.pxy
		groundPos					= {groundPos` & py = ~groundPos`.py}
		ballPosition				= convertToIso (move_point {dx=scale 0.5 flength,dy=scale 0.5 fwidth} groundPos)
		elevation					= move_point {zero & dy=scale (~fieldfit) football.ballPos.pz} ballPosition
		ball						= circle (pix (scale 2.8 radius_football))
	
	drawGoalAreas					:: !*Picture -> *Picture
	drawGoalAreas picture
	# picture						= drawLine (ppix (convertToIso {px = flength,py = y_north})) (ppix (convertToIso {px = x_east, py = y_north})) picture	// east north edge
	# picture						= drawLine (ppix (convertToIso {px = flength,py = y_south})) (ppix (convertToIso {px = x_east, py = y_south})) picture	// east south edge
	# picture						= drawLine (ppix (convertToIso {px = x_east, py = y_south})) (ppix (convertToIso {px = x_east, py = y_north})) picture	// east edge
	# picture						= drawLine (ppix (convertToIso {px = zero,   py = y_north})) (ppix (convertToIso {px = x_west, py = y_north})) picture	// west north edge
	# picture						= drawLine (ppix (convertToIso {px = zero,   py = y_south})) (ppix (convertToIso {px = x_west, py = y_south})) picture	// west south edge
	# picture						= drawLine (ppix (convertToIso {px = x_west, py = y_south})) (ppix (convertToIso {px = x_west, py = y_north})) picture	// west edge
	= picture
	where
		(y_north,y_south)			= (scale 0.5 fwidth + goal_area_depth + northPole, scale 0.5 fwidth + southPole - goal_area_depth)
		(x_west, x_east)			= (goal_area_depth, flength - goal_area_depth)
	
	drawPenaltyAreas				:: !*Picture -> *Picture
	drawPenaltyAreas picture
	# picture						= drawLine (ppix (convertToIso {px = flength,py = y_north})) (ppix (convertToIso {px = x_east, py = y_north}))    picture	// east north edge
	# picture						= drawLine (ppix (convertToIso {px = flength,py = y_south})) (ppix (convertToIso {px = x_east, py = y_south}))    picture	// east south edge
	# picture						= drawLine (ppix (convertToIso {px = x_east, py = y_south})) (ppix (convertToIso {px = x_east, py = y_north}))    picture	// east edge
	# picture						= drawLine (ppix (convertToIso {px = zero,   py = y_north})) (ppix (convertToIso {px = x_west, py = y_north}))    picture	// west north edge
	# picture						= drawLine (ppix (convertToIso {px = zero,   py = y_south})) (ppix (convertToIso {px = x_west, py = y_south}))    picture	// west south edge
	# picture						= drawLine (ppix (convertToIso {px = x_west, py = y_south})) (ppix (convertToIso {px = x_west, py = y_north}))    picture	// west edge
	# picture						= drawAt   (ppix east_spotIso) (circle (pix (scale fieldfit radius_penalty_spot)))                                picture	// east penalty spot
	# picture						= drawAt   (ppix (move_point (convertToIsoVector {dx=m (~h_dist),dy=m dyStart}) east_spotIso)) east_curve picture	// east curve
	# picture						= drawAt   (ppix west_spotIso) (circle (pix (scale fieldfit radius_penalty_spot)))                                picture	// west penalty spot
	# picture						= drawAt   (ppix (move_point (convertToIsoVector {dx=m h_dist,dy=m (~dyStart)}) west_spotIso)) west_curve picture	// west curve					                                     
	= picture
	where
		(y_north,y_south)			= (northPole + penalty_area_depth + scale 0.5 fwidth, southPole - penalty_area_depth + scale 0.5 fwidth)
		(x_west, x_east)			= (penalty_area_depth, flength - penalty_area_depth)
		angle						= arccosinus (h_dist / (toReal radius_penalty_area * fieldfit))
		h_dist						= (toReal penalty_area_depth - toReal penalty_spot_depth) * fieldfit
		east_spotIso				= convertToIso {px = flength - penalty_spot_depth, py = scale 0.5 fwidth}
		west_spotIso				= convertToIso {px =           penalty_spot_depth, py = scale 0.5 fwidth}
		circleStartWest     		= {px = scale (cosinus (angle - rad (0.5*pi))) radius_penalty_area, py = scale (sinus (angle - rad (0.5*pi))) radius_penalty_area}
		circleStartEast				= {px = scale (cosinus (angle + rad (0.5*pi))) radius_penalty_area, py = scale (sinus (angle + rad (0.5*pi))) radius_penalty_area}
		dyStart						= sqrt (rpaff * rpaff - h_dist * h_dist)
		rpaff						= toReal radius_penalty_area * fieldfit
		east_curve					= {polygon_shape = circle_shape radius_penalty_area (rad (pi/180.0)) (rad (0.5*pi) + angle) circleStartEast   (rad (0.5*pi) - angle)  []}
		west_curve					= {polygon_shape = circle_shape radius_penalty_area (rad (pi/180.0)) (angle - rad (0.5*pi)) circleStartWest (~(angle + rad (0.5*pi))) []}
	
	drawCornerAreas					:: !*Picture -> *Picture
	drawCornerAreas picture
	# picture						= drawAt (ppix (convertToIso {px = flength - radius_corner_kick_area, py = fwidth})) (corner (rad (1.0*pi) - a1) (rad (0.0*pi) + a1) True ) picture	// east south corner
	# picture						= drawAt (ppix (convertToIso {px =           radius_corner_kick_area, py = zero  })) (corner (rad (2.0*pi) - a1) (rad (1.0*pi) + a1) True ) picture	// west north corner
	# picture						= drawAt (ppix (convertToIso {px =           radius_corner_kick_area, py = fwidth})) (corner (rad (1.5*pi) + a2) (rad (0.5*pi) - a2) False) picture	// west south corner
	# picture						= drawAt (ppix (convertToIso {px = flength - radius_corner_kick_area, py = zero  })) (corner (rad (0.5*pi) + a2) (rad (1.5*pi) - a2) False) picture	// east north corner
	= picture
	where
		corner f t c				= { curve_oval      = circle (pix (scale fieldfit radius_corner_kick_area))
									  , curve_from      = toReal f
									  , curve_to        = toReal t
									  , curve_clockwise = c
									  }
		(a1,a2)						= (arctangens 0.5, arctangens 2.0)
		
	circle_shape					:: !Metre !Angle !Angle !Position !Angle ![Vector2] -> [Vector2]
	circle_shape r i angle base end vs
	| angle <= end					= reverse vs
	| otherwise						= circle_shape r i (angle-i) this end [{vx = thisIso.x - baseIso.x, vy = baseIso.y - thisIso.y} : vs]
	where
		this						= {px = scale (cosinus angle) r, py = scale (sinus angle) r}
		thisIso						= ppix (convertToIsoNormal this)
		baseIso						= ppix (convertToIsoNormal base)
		
		convertToIsoNormal {px,py}	= {px = scale fieldfit (px - py + fwidth), py = scale (0.5*fieldfit) (px + py)}
	
	convertToIso					:: !Position -> Position
	convertToIso pos				= { px = scale      fieldfit  (pos.px - yScaled pos + fwidth)
									  , py = scale (0.5*fieldfit) (pos.px + yScaled pos)
									  }
	
	yScaled							:: !Position -> Metre
	yScaled pos=:{px, py}
	| py <= scale 0.5 fwidth		= py + scale yscale d
	| otherwise						= py - scale yscale d
	where
		yscale		 				= (1.0 - (toReal px)/(toReal flength)) / factor
		d	 						= abs (py - scale 0.5 fwidth)
		factor						= 8.0
		
	convertToIsoVector				:: !RVector -> RVector
	convertToIsoVector {dx,dy}		= {dx = dx - dy, dy = scale 0.5 (dx+dy)}
	
	convertToIsoXNormal				:: !Real !Real -> Real
	convertToIsoXNormal x y			= x - y

	convertToIsoYNormal				:: !Real !Real -> Real
	convertToIsoYNormal x y			= (x + y) / 2.0

green								:== RGB {r=30,g=140,b=40}			// dark green works better on a beamer
black								:== Black							// black
ball_colour							:== Black

circle r							:== {oval_rx=r,oval_ry=r}
