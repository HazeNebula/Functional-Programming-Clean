implementation module Buffer

import StdEnvExt
import Footballer

buffer :: !FootballField !Home !Edge !FootballerID -> Footballer
buffer field home edge playerID = {defaultFootballer playerID & name   = "buffer"
                                                              , length = m 2.2
                                                              , skills = (Running, Gaining, Rotating)
                                                              , brain  = { memory = {curDir = if (edge == North) (rad (0.5*pi)) (rad (-0.5*pi))}
                                                                         , ai     = the_mind_of_a_buffer field home
                                                                         }
                                                              }

::	Memory	=	{ curDir :: !Angle
				}

the_mind_of_a_buffer :: !FootballField !Home !(!BrainInput,!Memory) -> (!BrainOutput,!Memory)
the_mind_of_a_buffer field home ({football,others,me},memory)
| dist ball me < maxGainReach me															// I can gain the ball
						= (GainBall,memory)
| dist ball me < m 30.0 && ballIsInZone														// I can not gain the ball, but I can run to it
						= (runTowardsBall,memory)
| abs me.pos.py > scale 0.5 field.fwidth - sidebuffer										// near the edge, turn around
						= (Move {velocity = ms 25.0,direction=new_direction} zero, {memory & curDir = new_direction})
						with
							new_direction	= if (me.speed.direction < zero) (rad (0.5*pi)) (rad (-0.5*pi))
| otherwise				= (Move {velocity = ms 25.0,direction=memory.curDir} zero,memory)	// no ball, no turning, just keep running
where
	sidebuffer			= scale 0.01 field.fwidth
	ball				= getFootball football [me:others]
	ball_x				= ball.ballPos.pxy.px
	ballIsInZone		= if (home == East) (isbetween ball_x (scale  0.15 field.flength) (scale  0.20 field.flength))
						                    (isbetween ball_x (scale -0.10 field.flength) (scale -0.10 field.flength))
	nextBallPos			= (nextpos ball).ballPos
	direction2ball		= bearing me.speed.direction me nextBallPos.pxy
	runTowardsBall		= Move {direction=direction2ball,velocity=ms 10.0} zero

nextpos :: !Football -> Football
nextpos ball=:{ballSpeed={vxy={velocity=v,direction=d},vz=v3},ballPos}
	= {ball & ballPos   = move_point3D {zero & dxy={dx=m (cosinus d * toReal v),dy=m (sinus d * toReal v)}} ballPos}
