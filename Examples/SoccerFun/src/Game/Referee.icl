implementation module Referee

import StdEnvExt
import matchGame
import Umpire
import NoReferee
//	When coding for all referees, include following modules:
import RefereeCoach_Rounds_Assignment
import RefereeCoach_Slalom_Assignment
import RefereeCoach_Passing_Assignment
import RefereeCoach_DeepPass_Assignment
import RefereeCoach_Keeper_Assignment

allAvailableReferees	:: [FootballField -> Referee]
allAvailableReferees	= [ umpire, NoReferee ]
//	When coding for all referees, use following list:
							++
						  [ RefereeCoach_Rounds
						  , RefereeCoach_Slalom
						  , RefereeCoach_Passing
						  , RefereeCoach_DeepPass
						  , RefereeCoach_Keeper
						  ]

instance nameOf Referee where nameOf {Referee | name} = name

defaultReferee			:: Referee
defaultReferee			= { name			= "Default"
						  , brain			= {memory = Void,ai = \(_,st) -> ([ContinueGame],st)}
						  , refActionPics	= []
						  }

defaultImage			:: !Match !RefereeAction !*env -> (!Bitmap,!*env)	| FileSystem env
defaultImage match action env
# bitmapf				= case action of
							(ReprimandPlayer  _ r)	= "ivanov_" +++ reprimandf r +++ ".bmp"
							(Hands            _)	= "hands.bmp"
							(OwnBallIllegally _)	= "ivanov_badluck.bmp"
							(TellMessage      _)	= "ivanov_look.bmp"
							(DirectFreeKick   _ p)	= ivanovf (p.px < zero)
							(GoalKick         h)	= ivanovf (h == West)
							(Corner           h _)	= ivanovf (h == West)
							(ThrowIn          h _)	= ivanovf (h == West)
							(Penalty          h)	= ivanovf (h == West)
							(Advantage        _)	= "ivanov_badluck.bmp"
							_						= "ivanov_fluit.bmp"
= case openBitmap ("afbeeldingen\\" +++ bitmapf) env of
	(Just bm,env)		= (bm,env)
	nothing				= abort "defaultImage: unable to load default picture.\n"
where
	reprimandf r		= case r of
							Warning    = "warning"
							YellowCard = "yellow"
							RedCard    = "red"
	ivanovf left		= "ivanov_wijst_" +++ if left "links" "rechts" +++ ".bmp"

defaultSoundFile		:: !RefereeAction -> Maybe String
defaultSoundFile action	= if (soundfilename == "") Nothing (Just ("sound\\"+++soundfilename))
where
	soundfilename		= defaultSoundFileName action
	
	defaultSoundFileName (Hands _)					= "stopBecauseOfFoul.wav"
	defaultSoundFileName (TackleDetected _)			= "tackles_ed.wav"
	defaultSoundFileName (DangerousPlay _)			= "tackles_ed.wav"
	defaultSoundFileName GameOver					= "endGameOrHalf.wav"
	defaultSoundFileName (GameCancelled _)			= "endGameOrHalf.wav"
	defaultSoundFileName EndHalf					= "endGameOrHalf.wav"
	defaultSoundFileName (Offside _)				= "offside.wav"
	defaultSoundFileName (GoalKick _)				= "ballOut.wav"
	defaultSoundFileName (Corner _ _)				= "ballOut.wav"
	defaultSoundFileName (ThrowIn _ _)				= "ballOut.wav"
	defaultSoundFileName (Goal _)					= "CenterKick.wav"
	defaultSoundFileName (OwnBallIllegally _)		= "wrongPosition2restartFrom.wav"
	defaultSoundFileName _							= ""

randomlessRefereeAI		:: (RefereeAI msg memory) -> RefereeAI msg (memory,RandomSeed)
randomlessRefereeAI f	= \(input,(memory,seed))	= let (decisions,memory`) = f (input,memory) in (decisions,(memory`,seed))

amnesiaRefereeAI		:: (RefereeAI msg RandomSeed) -> RefereeAI msg (memory,RandomSeed)
amnesiaRefereeAI f		= \(input,(memory,seed))	= let (decisions,seed`) = f (input,seed) in (decisions,(memory,seed`))

witlessRefereeAI		:: (RefereeAI` msg) -> RefereeAI msg (memory,RandomSeed)
witlessRefereeAI f		= \(input,state)			= (f input,state)
