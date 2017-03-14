implementation module NoReferee

import Referee, RefereeFunctions

NoReferee :: !FootballField -> Referee
NoReferee field	= { name  = "NoReferee"
				  , brain = { memory = Memory
				            , ai     = amnesiaRefereeAI (brain field)
				            }
				  , refActionPics    = []
				  }

::	Memory = Memory

brain :: !FootballField !(!RefereeInput,!RandomSeed) -> (!RefereeOutput,!RandomSeed)

//	Referee checks whether the game is completely over:
brain field ({RefereeInput | playingTime},seed)
| playingTime <= zero		= ([GameOver],seed)

//	Referee checks whether the ball has exited the football field.
//	A coin is flipped to decide which team can resume game.
brain field (input,seed)
# (r,seed)					= random seed
# team						= if (isEven r) West East
| isJust ball_exit			= (if is_throw_in [ThrowIn team exit_pos] [GoalKick team],seed)
where
	ball_exit				= ball_left_field_at field input
	exit_pos				= fromJust ball_exit
	is_throw_in				= abs exit_pos.py >= scale 0.5 field.fwidth

//	And that's about it...
brain _ (_,seed)			= ([],seed)
