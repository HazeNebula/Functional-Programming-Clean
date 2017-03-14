definition module FootballerFunctions

/**	Functions for creating miniscule brains:
*/

import Footballer

//	The core functions that do not require a memory:
returnAI`			:: !FootballerAction -> FootballerAI`	// just return the action
halt`				::                      FootballerAI`	// halt lets the footballer stand quite still
rotate`				:: !Angle            -> FootballerAI`	// rotate over given angle
ahead`				:: !Velocity         -> FootballerAI`	// follow your nose with given velocity/
fix`				:: !Position !Metre  -> FootballerAI`	// fix p d lets the footballer run to p, with a precision of d
kick`				:: !Position         -> FootballerAI`	// kick p lets the footballer kick the ball to position p, if in kicking range
track_ball`			:: !Metre            -> FootballerAI`	// track_ball` d lets the footballer move to the ball, with a precision of d

amnesia				:: FootballerAI`     -> FootballerAI m

//	The derived functions that do not care about their memory (the amnesia versions of the above functions):
returnAI			:: (FootballerAction -> FootballerAI m)
halt				:: (                    FootballerAI m)
rotate				:: (Angle            -> FootballerAI m)
ahead				:: (Velocity         -> FootballerAI m)
fix					:: (Position Metre   -> FootballerAI m)
kick				:: (Position         -> FootballerAI m)
track_ball			:: (Metre            -> FootballerAI m)

centerOfGoal		:: !Home !FootballField -> Position
(team)      infix 9	:: !Footballer ![Footballer] -> [Footballer]
(opponents) infix 9	:: !Footballer ![Footballer] -> [Footballer]

getBall				:: !BrainInput -> Football
