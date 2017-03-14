implementation module FootballerFunctions

import Footballer

returnAI`					:: !FootballerAction -> FootballerAI`
returnAI` action			= const action

move`						:: !Speed !Angle -> FootballerAI`
move` speed angle			= returnAI` (Move speed angle)

halt`						:: FootballerAI`
halt`						= move` zero zero

rotate`						:: !Angle -> FootballerAI`
rotate` angle				= move` zero angle

ahead`						:: !Velocity -> FootballerAI`
ahead` v					= \input=:{me} -> move` {direction=me.nose,velocity=v} zero input

fix`						:: !Position !Metre -> FootballerAI`
fix` point eps				= \input=:{me} ->
  let distance				= dist            me point
      angle					= bearing zero    me point
      rotate				= bearing me.nose me point
      v						= ms (max 6.0 (toReal distance))
  in if (distance <= eps)
        (halt` input)
  	    (move` {direction=angle,velocity=v} rotate input)

kick`						:: !Position -> FootballerAI`
kick` point 				= \input=:{me} ->
  let ball					= getBall input
      angle					= bearing zero me point
      v						= ms (2.0 * (toReal (dist me point)))
  in if (dist me ball <= maxKickReach me)
  		(KickBall {vxy = {direction=angle,velocity=v},vz=ms 1.0})
  		(halt` input)

track_ball`					:: !Metre -> FootballerAI`
track_ball` eps				= \input -> fix` (getBall input).ballPos.pxy eps input

amnesia						:: FootballerAI` -> FootballerAI m
amnesia f                   = \(input,m) -> (f input,m)

returnAI					:: (FootballerAction -> FootballerAI m)
returnAI					= amnesia o returnAI`

move						:: (Speed Angle -> FootballerAI m)
move						= \speed angle -> amnesia (move` speed angle)

halt						:: (FootballerAI m)
halt						= amnesia halt`

rotate						:: (Angle -> FootballerAI m)
rotate						= amnesia o rotate`

ahead						:: (Velocity -> FootballerAI m)
ahead						= amnesia o ahead`

fix							:: (Position Metre -> FootballerAI m)
fix							= \point eps -> amnesia (fix` point eps)

kick						:: (Position -> FootballerAI m)
kick						= amnesia o kick`

track_ball					:: (Metre -> FootballerAI m)
track_ball					= \eps -> amnesia (track_ball` eps)

centerOfGoal				:: !Home !FootballField -> Position
centerOfGoal home field		= {zero & px = if (home==West) (~half_length) half_length}
where
	half_length				= scale 0.5 field.flength

(team) infix 9				:: !Footballer ![Footballer] -> [Footballer]
(team) player players		= filter (sameClub player) players

(opponents) infix 9			:: !Footballer ![Footballer] -> [Footballer]
(opponents) player players	= filter (not o (sameClub player)) players

getBall						:: !BrainInput -> Football
getBall {football,me,others}= getFootball football [me : others]

:: HomeM m					= { home :: !Home, mem :: !m }

educate`					:: (Home -> FootballerAI`) -> FootballerAI (HomeM m)
educate` home_ai			= \(input=:{referee},memory) ->
  let new_home				= if (any isEndHalf referee) other id memory.home
      action				= home_ai new_home input
  in  (action, {memory & home = new_home})

educate						:: (Home -> FootballerAI m) -> FootballerAI (HomeM m)
educate home_ai				= \(input=:{referee},memory) ->
  let new_home				= if (any isEndHalf referee) other id memory.home
      (action,new_memory)	= home_ai new_home (input,memory.mem)
  in  (action, {memory & home = new_home, mem = new_memory})
