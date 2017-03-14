implementation module RefereeCoach_Rounds_Assignment

import Referee

RefereeCoach_Rounds				:: !FootballField -> Referee
RefereeCoach_Rounds field		= { name			= "RefereeCoach_Rounds"
								  , brain			= { memory	= mkMemory
								  					  , ai		= randomlessRefereeAI (brain field)
								  					  }
								  , refActionPics	= []
								  }

::	Memory						= { players			:: ![(FootballerID, [Checkpoint])]	// players and their checkpoints
								  , stage			:: !Stage							// current stage of training
								  }
::	Stage						= Begin | Rounds | End Success
::	Checkpoint					= { base			:: !Position
								  , distance		:: !Metre
								  , check			:: !Bool
								  }

mkMemory						:: Memory
mkMemory						= { players			= []
								  , stage			= Begin
								  }

fail							:: Memory -> Memory
fail memory						= {memory & stage = End Fail}

ok								:: Memory -> Memory
ok memory						= {memory & stage = End Success}

rounds							:: Memory -> Memory
rounds memory					= {memory & stage = Rounds}

instance toPosition3D Checkpoint
where    toPosition3D {base}	= toPosition3D base

cornerCheckpoints				:: !FootballField !Metre -> [Checkpoint]
cornerCheckpoints field d		= [  { base = {px = x, py = y}, distance = d, check = False } 
								  \\ x <- [scale -0.5 field.flength, scale  0.5 field.flength]
								   , y <- [scale  0.5 field.fwidth,  scale -0.5 field.fwidth ]
								  ]

checkPoint						:: !Position !Checkpoint -> Checkpoint
checkPoint pos checkpoint		= {checkpoint & check = checkpoint.check || dist pos checkpoint <= checkpoint.distance}

checkpointsCleared				:: ![Checkpoint] -> Bool
checkpointsCleared checks		= and [check \\ {check} <- checks]

distanceFromEdges				:== m 5.0

isIllegal						:: !FootballField !Position !Metre -> Bool
isIllegal field pos distance	= point_in_rectangle ({px = scale -0.5 field.flength + distance, py = scale -0.5 field.fwidth + distance}
								                     ,{px = scale  0.5 field.flength - distance, py = scale  0.5 field.fwidth - distance}
								                     ) pos

brain							:: !FootballField !(!RefereeInput,!Memory) -> (!RefereeOutput,!Memory)

//	Assignment has started. Locate all players and their checkpoints.
brain field ({RefereeInput | team1,team2},memory=:{stage = Begin})
| isEmpty players				= verdict [TellMessage "No players selected."] (fail memory)
| otherwise						= ([], rounds {memory & players = players})
where
	players						= [(player.playerID,cornerCheckpoints field distanceFromEdges) \\ player <- team1 ++ team2]

//	Assignment is in the running stage. Monitor players and provide feedback.
brain field ({RefereeInput | playingTime=time,unittime=dt,team1,team2},memory=:{players,stage = Rounds})
| time <= minutes dt			= verdict msg` (next next_memory)
								with
									(msg`,next)	= if (all (checkpointsCleared o snd) checked) 
													 ([], ok)
													 ([TellMessage "Not all checkpoints have been cleared."],fail)
| isEmpty illegal_positions		= ([], next_memory)
| otherwise						= ([TellMessage (length illegal_positions +++> " players have moved too far from edge.")], fail next_memory)
where
	next_memory					= { memory & players = checked }
	checked						= [ (playerID, map (checkPoint pos) checkpoints) \\ (playerID,checkpoints) <- players
								                                                  & pos                    <- positions
								  ]
	positions					= [ (find_player playerID all_players).pos       \\ (playerID,_)           <- players
								  ]
	illegal_positions			= [ (playerID,pos)                               \\ (playerID,_)           <- players
								                                                  & pos                    <- positions
								                                                  | isIllegal field pos distanceFromEdges
								  ]
	all_players					= team1 ++ team2

//	Assignment has ended. Stop training session and give final verdict.
brain _ (_,memory)				= verdict [] memory

verdict							:: ![RefereeAction] !Memory -> (![RefereeAction],!Memory)
verdict actions memory=:{stage=End how}
								= (actions ++ [TellMessage msg,GameOver], memory)
where
	msg							= if (how == Success) "Well done! Move on to next exercise."
								                      "Improve your assignment and try again."
verdict actions memory			= (actions ++ [GameOver], fail memory)

find_player						:: FootballerID [Footballer] -> Footballer
find_player playerID players	= case filter (identify_player playerID) players of
									[player : _]	= player
									no_one			= abort ("player " <+++ playerID <+++ " lost in oblivion...")
