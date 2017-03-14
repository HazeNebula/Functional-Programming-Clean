definition module matchGame

/** This module defines the match and tournament data structures.
*/
import matchLog, guiInterface, render

::	FootballGame
	=	{ match				:: !Match						// the football match to be played
		, actionPics		:: !ActionPics					// the action-images
		, history			:: !History						// recent history of game
		, frames			:: !Int							// nr of frames so far (reset to zero every second)
		, options			:: !Options						// options of football game
		, logging			:: !WhatToLog					// logging options
		}

::	Options
	=	{ closeReferee		:: !Bool						// automatically close referee dialog after one second (True - default) or by user (False)
		, showSplash		:: !Bool						// show splash screen at opening (False - default) or do (True)
		, displaySpeed		:: !DisplaySpeed				// slow, normal or fast-play (Normal - default)
		, showReferee		:: !Bool						// show referee-intermezzo (True - default) or not (False)
		, playingTime		:: !PlayingTime					// default playingtime (defaultPlayingTime)
		, renderStyle       :: !RenderStyle					// the chosen render style (flatland style is default)
		}
instance toString   Options
instance fromString Options
instance ==         Options

::	History
	=	{ time				:: !Seconds						// time in seconds of length history
		, past				:: ![Match]						// the recent history
		} 

/**	incFrames game increases the frames count of game.
*/
incFrames					:: !FootballGame -> FootballGame

/*	defaultPlayingTime returns recommended playing time
*/
defaultPlayingTime			:: PlayingTime

/**	defaultOptions returns default options.
*/
defaultOptions				:: Options

/*	timeLeft is True if the game has not finished
*/
timeLeft					:: !FootballGame -> Bool

/**	getOptions env reads the options file (if present) and returns its content.
		If no options file was found, it is created and filled with default values.
	setOptions options stores the options in the options file.
*/
getOptions					:: !*env -> (!Options,!*env)	| FileSystem env
setOptions					:: !Options !*env -> *env		| FileSystem env

::	Competition				=	{ results		:: ![[Maybe Score]]	// teams x teams matrix of match results (note: team x team -> Nothing)
								, west			:: ![ClubName]		// names of participating teams (west side)
								, east			:: ![ClubName]		// names of participating teams (east side)
								, usedRandomSeed:: !RandomSeed		// the seed that is used for computing the matches
								}
::	Ranking					:== AssocList ClubName Rank
::	Rank					=	{ matchpoints	:: !Int				// number of matchpoints   (>= 0)
								, goals_scored	:: !Int				// number of scored goals  (>= 0)
								, goals_against	:: !Int				// number of goals against (>= 0)
								}
instance zero Rank
instance ==   Rank
instance <    Rank
instance +    Rank

/** competition teams field referee time rs
		computes an entire competition between all teams in teams.
		Each match uses the same referee and same initial random seed value rs.
*/
competition					:: ![Home FootballField -> Team] !FootballField !Referee !PlayingTime !RandomSeed -> Competition

/** computeMatch match
		computes an entire match between the currently selected team1 and team2.
*/
computeMatch				:: !Match -> Score

/** ranking competition
		computes the ranking of all teams that have participated in competition.
*/
//ranking						:: !Competition -> Ranking
ranking						:: ![ClubName] ![Maybe Score] -> Ranking

/** checkCompetitionFile west_team_names rs env
		checks whether there is a competition backup file present for the current set
		of teams (assuming they start on the West home side) and initial random seed value rs
		for computing matches. 
		If not, then such a file is created, and the same random seed value and empty list of scores is returned. 
		If so, then the currently stored random seed value and list of scores is returned.
*/
checkCompetitionFile		:: ![ClubName] !RandomSeed !*env -> (!(!RandomSeed,![Maybe Score]),!*env) | FileSystem env

/** appendMatchToCompetitionFile west east env
		appends an empty entry of a match between west versus east in the competition backup file.
		It also returns the file pointer to allow a correct update in updateMatchToCompetitionFile.
*/
appendMatchToCompetitionFile:: !ClubName !ClubName !*env -> (!Int,!*env) | FileSystem env


/** updateMatchToCompetitionFile west east score filepointer env
		updates the line that starts at filepointer in the competition backup file with the result 
		of the match between west versus east.
*/
updateMatchToCompetitionFile:: !ClubName !ClubName !(Maybe Score) !Int !*env -> *env | FileSystem env
