definition module Referee

/**	The referee data type, and all available referees within Soccer-Fun.
*/
import Footballer, GamePicture, matchGame

::	Referee					= E.memory:
								{ name			:: !String
								, brain			:: !Brain (RefereeAI RefereeOutput (memory,RandomSeed)) memory
								, refActionPics	:: ![Path]
								}
::	RefereeAI  msg memory	:== (RefereeInput,memory) -> (msg,memory)
::	RefereeAI` msg			:== RefereeInput -> msg
::	RefereeInput			=	{ playingTime	:: !PlayingTime			// the duration of an entire match
								, unittime		:: !TimeUnit			// the time unit of a single simulation step
								, theBall		:: !FootballState		// the whereabouts of the football
								, playingHalf	:: !Half				// first or second half; team1 is team that starts game on West; team2 is other team
								, team1			:: !Team				// team1
								, team2			:: !Team				// team2
								, lastContact	:: !Maybe FootballerID	// last player who has played the ball
								}
::	RefereeOutput			:== [RefereeAction]

instance nameOf Referee

defaultReferee		:: Referee

allAvailableReferees:: [FootballField -> Referee]
defaultImage		:: !Match !RefereeAction !*env -> (!Bitmap,!*env)	| FileSystem env
defaultSoundFile	:: !RefereeAction -> Maybe String

/**	Wrapper functions for simpler referee brains:
*/
randomlessRefereeAI	:: (RefereeAI  msg memory)		-> RefereeAI msg (memory,RandomSeed)
amnesiaRefereeAI	:: (RefereeAI  msg RandomSeed)	-> RefereeAI msg (memory,RandomSeed)
witlessRefereeAI	:: (RefereeAI` msg)				-> RefereeAI msg (memory,RandomSeed)
