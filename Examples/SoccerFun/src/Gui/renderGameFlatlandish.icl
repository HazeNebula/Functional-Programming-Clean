implementation module renderGameFlatlandish

import matchControl, render
import StdIOCommon, StdPSt

renderFlatland				:: RenderStyle
renderFlatland				= { name = "Flatland"
							  , look = flatland
							  }

/**	flatland match updSt
		renders a complete match, given the current dimensions of the drawing environment (updSt).
*/
flatland					:: !Match !SelectState !UpdateState !*Picture -> *Picture
flatland match=:{theField,theBall,team1,team2} _ updSt=:{newFrame} picture
# picture					= setPenColour Black                                  picture
# picture					= fill {corner1={x=pixl,y=zero},corner2={x=w,y=pixw}} picture		// exterior right
# picture					= fill {corner1={x=zero,y=pixw},corner2={x=w,y=h}}    picture		// exterior bottom
# picture					= unfill field_rectangle                              picture		// football field
# picture					= setPenColour White                                  picture
# picture					= draw field_rectangle                                picture		// field outline
# picture					= drawMiddleArea                                      picture		// middle line, circle and spot
# picture					= drawGoals                                           picture		// goals
# picture					= drawGoalAreas                                       picture		// goal areas
# picture					= drawPenaltyAreas                                    picture		// penalty areas
# picture					= drawCornerAreas                                     picture		// corner areas
# picture					= foldr (drawPlayer WestColour)                       picture team1	// team1 players
# picture					= foldr (drawPlayer EastColour)                       picture team2	// team2 players
# picture					= drawBall (getFootball theBall (team1 ++ team2))     picture		// football
= picture
where
	{w,h}					= rectangleSize newFrame
	{fwidth,flength}		= theField
	field_rectangle			= {corner1=ppix {px=scale -0.5 flength,py=scale 0.5 fwidth}, corner2=ppix {px=scale 0.5 flength,py=scale -0.5 fwidth}}
	ratio					= min ((toReal w) / (toReal flength)) ((toReal h) / (toReal fwidth))
	pixw					= pix fwidth
	pixl					= pix flength
	pix  x					= toInt (ratio * toReal x)
	pixx x					= pixl / 2 + pix x						// convert metres   to pixels
	pixy y					= pixw / 2 - pix y
	ppix {px,py}			= {x  = pixx px, y  = pixy py}			// convert Position to pixels
	vpix {dx,dy}			= {vx = pix  dx, vy = pix  dy}			// convert RVector  to pixels
	(northPole,southPole)	= goal_poles theField
	
	drawPlayer				:: !Colour !Footballer !*Picture -> *Picture
	drawPlayer colour fb=:{name,nose=nose_dir,effect,pos} picture
	# picture				= drawName          name picture
	| perhaps isOnTheGround effect
							= drawLyingPlayer    fb  picture
	| otherwise				= drawStandingPlayer fb  picture
	where
		player_width		= m 0.5
		body_circle			= circle (pix player_width)
		bearing_circle		= circle (pix (m 0.3))
		nose_dir_vector		= {dx = m (cosinus nose_dir), dy = m (sinus nose_dir) }
		offset				= {vx = -1, vy = -1}
		nose_dir_point k	= move_point (scale k nose_dir_vector) pos
		
		drawName name picture
		# picture			= setPenColour White                      picture
		# picture			= drawAt (ppix (move_point one pos)) name picture
		= picture
		
		drawLyingPlayer fb=:{length} picture
		# (w,picture)		= getPenSize                                                        picture
		# picture			= setPenSize (pix player_width+2)                                   picture
		# picture			= setPenColour Black                                                picture
		# picture			= draw (body_line offset)                                           picture
		# picture			= setPenSize (pix player_width)                                     picture
		# picture			= setPenColour colour                                               picture
		# picture			= draw (body_line zero)                                             picture
		# picture			= setPenSize w                                                      picture
		# picture			= fillAt (ppix (nose_dir_point (toReal length+0.2))) bearing_circle picture
		# picture			= setPenColour Black                                                picture
		# picture			= drawAt (ppix (nose_dir_point (toReal length+0.2))) bearing_circle picture
		= picture
		where
			body_line v		= { line_end1 = movePoint v (ppix pos)
							  , line_end2 = movePoint v (ppix (nose_dir_point (toReal length))) 
							  }
		
		drawStandingPlayer fb picture
		# picture			= setPenColour colour                               picture
		# picture			= fillAt (ppix pos) body_circle                     picture					// body
		# picture			= fillAt (ppix (nose_dir_point 1.0)) bearing_circle picture					// nose direction
		# picture			= setPenColour Black                                picture					// surrounding circle
		# picture			= drawAt (ppix pos) body_circle                     picture
		# picture			= drawAt (ppix (nose_dir_point 1.0)) bearing_circle picture					// nose direction
		= picture
			
	drawMiddleArea			:: !*Picture -> *Picture
	drawMiddleArea picture
	# picture				= drawLine (ppix {zero & py = scale 0.5 fwidth}) (ppix {zero & py = scale -0.5 fwidth}) picture
	# picture				= drawAt (ppix zero) (circle (pix radius_centre_circle)) picture			// middle area
	# picture				= drawAt (ppix zero) (circle (pix radius_centre_spot))   picture			// center spot
	= picture
	
	drawGoals				:: !*Picture -> *Picture
	drawGoals picture
	# picture				= drawAt (ppix {px=scale  0.5 flength-goal_indent,py=northPole}) h_vector picture		// east north pole
	# picture				= drawAt (ppix {px=scale  0.5 flength-goal_indent,py=southPole}) h_vector picture		// east south pole
	# picture				= drawAt (ppix {px=scale -0.5 flength,            py=northPole}) h_vector picture		// west north pole
	# picture				= drawAt (ppix {px=scale -0.5 flength,            py=southPole}) h_vector picture		// west south pole
	= picture
	where
		goal_indent			= m 1.0
		h_vector			= vpix {zero & dx=goal_indent}
	
	drawGoalAreas			:: !*Picture -> *Picture
	drawGoalAreas picture
	# picture				= drawAt (ppix {px=scale 0.5 flength-goal_area_depth,   py=northPole+goal_area_depth}) h_vector picture			// east north edge
	# picture				= drawAt (ppix {px=scale 0.5 flength-goal_area_depth,   py=southPole-goal_area_depth}) h_vector picture			// east south edge
	# picture				= drawAt (ppix {px=scale 0.5 flength-goal_area_depth,   py=northPole+goal_area_depth}) v_vector picture			// east edge
	# picture				= drawAt (ppix {px=scale -0.5 flength,                  py=northPole+goal_area_depth}) h_vector picture			// west north edge
	# picture				= drawAt (ppix {px=scale -0.5 flength,                  py=southPole-goal_area_depth}) h_vector picture			// west south edge
	# picture				= drawAt (ppix {px=scale -0.5 flength + goal_area_depth,py=northPole+goal_area_depth}) v_vector picture			// west edge
	= picture
	where
		h_vector			= vpix {zero & dx=goal_area_depth}
		v_vector			= vpix {zero & dy=scale 2.0 goal_area_depth + goal_width}
	
	drawPenaltyAreas		:: !*Picture -> *Picture
	drawPenaltyAreas picture
	# picture				= drawAt (ppix {px=scale 0.5 flength-penalty_area_depth,py=northPole+penalty_area_depth}) h_vector picture		// east north edge
	# picture				= drawAt (ppix {px=scale 0.5 flength-penalty_area_depth,py=southPole-penalty_area_depth}) h_vector picture		// east south edge
	# picture				= drawAt (ppix {px=scale 0.5 flength-penalty_area_depth,py=northPole+penalty_area_depth}) v_vector picture		// east edge
	# picture				= drawAt (ppix east_spot) (circle (pix radius_penalty_spot))                                       picture		// east penalty spot
	# picture				= drawAt (ppix (move_point {dx= ~h_dist,dy= scale (~(sinus angle)) radius_penalty_area} east_spot))				// east curve
							         {curve_oval      = circle (pix radius_penalty_area)
							         ,curve_from      = pi + angle
							         ,curve_to        = pi - angle
							         ,curve_clockwise = True } picture
	# picture				= drawAt (ppix {px=scale -0.5 flength,                     py=northPole+penalty_area_depth}) h_vector picture	// west north edge
	# picture				= drawAt (ppix {px=scale -0.5 flength,                     py=southPole-penalty_area_depth}) h_vector picture	// west south edge
	# picture				= drawAt (ppix {px=scale -0.5 flength + penalty_area_depth,py=northPole+penalty_area_depth}) v_vector picture	// west edge
	# picture				= drawAt (ppix west_spot) (circle (pix radius_penalty_spot))                                          picture	// west penalty spot
	# picture				= drawAt (ppix (move_point {dx=h_dist,dy= scale (~(sinus angle)) radius_penalty_area} west_spot))				// west curve
							         {curve_oval      = circle (pix radius_penalty_area)
							         ,curve_from      = ~angle
							         ,curve_to        = angle
							         ,curve_clockwise = False } picture
	= picture
	where
		h_vector			= vpix {zero & dx=penalty_area_depth}
		v_vector			= vpix {zero & dy=scale 2.0 penalty_area_depth + goal_width}
		angle				= arccosinus ((toReal h_dist)/(toReal radius_penalty_area))
		h_dist				= penalty_area_depth - penalty_spot_depth
		east_spot			= {zero & px=scale  0.5 flength - penalty_spot_depth}
		west_spot			= {zero & px=scale -0.5 flength + penalty_spot_depth}
	
	drawCornerAreas			:: !*Picture -> *Picture
	drawCornerAreas picture
	# picture				= drawAt (ppix {px=scale -0.5 flength + radius_corner_kick_area,py=scale  0.5 fwidth}) (corner 0.0 (1.5*pi) True ) picture	// west north corner
	# picture				= drawAt (ppix {px=scale -0.5 flength + radius_corner_kick_area,py=scale -0.5 fwidth}) (corner 0.0 (0.5*pi) False) picture	// west south corner
	# picture				= drawAt (ppix {px=scale  0.5 flength - radius_corner_kick_area,py=scale  0.5 fwidth}) (corner pi  (1.5*pi) False) picture	// east north corner
	# picture				= drawAt (ppix {px=scale  0.5 flength - radius_corner_kick_area,py=scale -0.5 fwidth}) (corner pi  (0.5*pi) True ) picture	// east south corner
	= picture
	where
		corner f t c		= {curve_oval=circle (pix radius_corner_kick_area),curve_from=f,curve_to=t,curve_clockwise=c}
	
	drawBall				:: !Football !*Picture -> *Picture
	drawBall ball picture
	# picture				= setPenColour ball_colour picture
	# picture				= fillAt (ppix ball.ballPos.pxy) (circle (pix (scale (2.8*toReal radius_football) (m 1.0 + scale 0.2 ball.ballPos.pz)))) picture
	= picture

ball_colour					:== Black
circle r					:== {oval_rx=r,oval_ry=r}
