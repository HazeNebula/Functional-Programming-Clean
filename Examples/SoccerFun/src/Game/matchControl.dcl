definition module matchControl

/**	This module defines the logical behavior of Soccer-Fun. 
	The core function is stepMatch, which computes a single, complete transition of a match.
*/
import Referee

::	Match			= { team1		:: !Team				// team1
					  , team2		:: !Team				// team2
					  , theBall		:: !FootballState		// the whereabouts of the football
					  , theField	:: !FootballField		// the football field
					  , theReferee	:: !Referee				// the referee
					  , playingHalf	:: !Half				// first half or second half team1 plays West at first half and East at second half
					  , playingTime	:: !PlayingTime			// todo: add a boolean gameOver, playingtime will not walk back to zero and its up to the referee at which time he is to end the game
					  , score		:: !Score				// the score
					  , nextRandomP	:: !St RandomSeed P		// generate pseudo random value
					  , seed		:: !RandomSeed			// random seed for generating pseudo random values
					  , unittime	:: !TimeUnit			// the time unit of a single simulation step
					  , lastContact :: !Maybe FootballerID	// the player who has the ball the last time (ball can have bounced against this player)
					  }
::	PlayingTime		:== Minutes
::	Score			:== (!NrOfGoals,!NrOfGoals)				// (goals by Team1, goals by Team2)
::	NrOfGoals		:== Int									// zero <= nr of goals

::	TimeUnit		:== Seconds								// time unit in sec.
::	Seconds

s					:: !Real -> Seconds						// (s x) represents x seconds of time
instance zero		Seconds
instance ==			Seconds
instance <			Seconds
instance +			Seconds
instance -			Seconds
instance minutes	Seconds
instance toReal     Seconds
instance scale      Seconds
instance toString   Seconds

doSoccerFun			:: !*World -> *World
setMatchStart		:: !Team !Team !FootballField !Referee !PlayingTime !RandomSeed -> Match
stepMatch			:: !Match -> (!(![RefereeAction],!AssocList FootballerID FootballerAction),!Match)
