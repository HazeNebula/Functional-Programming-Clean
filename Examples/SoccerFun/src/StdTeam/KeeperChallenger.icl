implementation module KeeperChallenger

import Team

keeperChallenger					:: !FootballerID -> Footballer
keeperChallenger playerID			= { defaultFootballer playerID & name   = "Zidane"
							                                       , length = m 2.2
							                                       , skills = (Gaining, Catching, Kicking)
							                                       , brain  = {memory = {waitToKick = 0}, ai = mind}
							          }

::	Memory							= { waitToKick	:: !Int }

decreaseWait						:: !Memory -> Memory
decreaseWait memory=:{waitToKick}	= {memory & waitToKick = max 0 (waitToKick-1)}

startWaiting						:: !Memory -> Memory
startWaiting memory					= {memory & waitToKick = 20}

mind								:: !(!BrainInput,!Memory) -> (!BrainOutput,!Memory)
mind ({football,others,me},memory)
| memory.waitToKick > 0				= (Move zero zero,decreaseWait memory)
| ballIsGainedBy me.playerID football
									= (KickBall {vxy={direction=bearing zero me next_pos,velocity=ms (max 5.0 (toReal (dist me next_pos)))},vz=ms 1.0},startWaiting memory)
| ballIsFree football && i_am_closest_to_ball
	| dist_to_free_ball <= maxGainReach me
									= (GainBall,memory)
	| otherwise						= (Move {direction=angle_with_free_ball,velocity=ms 4.0} (angle_with_free_ball - me.nose),memory)
| otherwise							= (Move zero zero,memory)
where
	team_players					= filter (sameClub me) others
	my_nr							= me.playerID.playerNr
	next_nr							= ((my_nr-1) rem (length team_players)) + 2
	next_player						= hd (filter (identify_player {me.playerID & playerNr=next_nr}) team_players)
	next_pos						= next_player.pos
	free_ball						= getFootball football [me:others]
	angle_with_free_ball			= bearing zero me free_ball
	dist_to_free_ball				= dist me free_ball
	i_am_closest_to_ball			= dist_to_free_ball <= minList (map (dist free_ball) team_players)
