implementation module matchLog

import StdEnvExt, fileIO
import matchControl

logFile						:: String
logFile						= "SoccerFun.log"

getWhatToLog				:: !*env -> (!WhatToLog,!*env) | FileSystem env
getWhatToLog env
	= case readFile fileStr env of
		(Just line,env)		= (fromString line,env)
		(nothing,  env)		= (defaultLogging, env)
where
	fileStr					= "logMatch.conf"

instance toString WhatToLog where 
	toString l = "{footballerActions=" <+++ l.footballerActions +++ 
				 ",fbPositions="       <+++ l.fbPositions       +++ 
				 ",refEvents="         <+++ l.refEvents         +++ 
				 ",ballPosition="      <+++ l.ballPosition      +++ 
				 "}"
instance fromString WhatToLog where
	fromString str
		# (okActions,  actions,  str)			= isBool (literal "{footballerActions=" str)
		# (okPositions,positions,str)			= isBool (literal ",fbPositions="       str)
		# (okRefEvents,events,   str)			= isBool (literal ",refEvents="         str)
		# (okPositions,pos,      str)			= isBool (literal ",ballPosition="      str)
		| okActions && okPositions && okRefEvents && okPositions && str=="}"
			= { footballerActions				= actions
			  , fbPositions						= positions
			  , refEvents						= events
			  , ballPosition					= pos
			  }
		| otherwise
			= defaultLogging
	where
		literal literal str
		| stringStarts str literal				= str%(size literal,size str-1)
		| otherwise								= str
		isBool str
		| stringStarts str "True"				= (True,True, str%(4,size str-1))
		| stringStarts str "False"				= (True,False,str%(5,size str-1))
		| otherwise								= (False,undef,str)

defaultLogging		= { footballerActions		= False
					  , fbPositions				= False
					  , refEvents				= False
					  , ballPosition			= False
					  }


/*	logMatch logging match refereeActions succeededActions env
		writes the log fields indicated in logging from the match and actions to disk.
*/
logMatch :: !WhatToLog !Match ![RefereeAction] !(AssocList FootballerID FootballerAction) !*env -> *env | FileSystem env
logMatch logging match_to_log=:{Match | team1,team2,playingTime,theBall} refereeActions succeededActions env
	| logging.footballerActions || logging.fbPositions || logging.refEvents || logging.ballPosition
		# env				= writeFile True logFile ("BEGIN STEP "<+++playingTime<+++"\n") env
		# env				= if (logging.footballerActions) (writeFile True logFile (logActions    allPlayers succeededActions) env) env
		# env				= if (logging.fbPositions)       (writeFile True logFile (logPositions  allPlayers)                  env) env
		# env				= if (logging.refEvents)         (writeFile True logFile (logRefereeActions refereeActions)          env) env
		# env				= if (logging.ballPosition)      (writeFile True logFile ("BallPos: " <+++ ball.ballPos +++ "\n")    env) env
		# env				= writeFile True logFile ("END STEP " <+++ playingTime <+++ "\n") env
		= env
	| otherwise
		= env
where
	allPlayers				= team1 ++ team2
	ball					= getFootball theBall allPlayers 

	logActions :: ![Footballer] !(AssocList FootballerID FootballerAction) -> String
	logActions _ []		= ""
	logActions allPlayers [(playerID=:{clubName,playerNr},action) : mfals]
						= "FootballerWithAction\n\tTeam: "     <+++ 
						  clubName                             <+++ 
						  "\n\tNumber: "                       <+++ 
						  playerNr                             <+++ 
						  "\n\tName: "                         <+++ 
						  nameOf self                          <+++ 
						  "\n\tAction: "                       <+++ 
						  improveString (toString action)      <+++ 
						  "\n"                                 <+++ logActions allPlayers mfals
	where
		self			= find1 (identify_player playerID) allPlayers
		
	improveString		= (replaceInString "}\"" "}") o (replaceInString "\"{" "{") o (replaceInString "\\\"" "\"")

	logPositions :: ![Footballer] -> String
	logPositions fbs	= "POSITIONS:\n" <+++ foldl printFootballer "" fbs
	where
		printFootballer str {playerID={clubName,playerNr},name,pos}
						= str <+++ "\t(" <+++ clubName <+++ ",(" <+++ playerNr <+++"," <+++ name <+++")):\t" <+++ pos <+++ "\n"

	logRefereeActions :: ![RefereeAction] -> String
	logRefereeActions []				= ""
	logRefereeActions refls				= "REFEREE_ACTIONS:" <+++ logRefereeActions` refls <+++ "\n"
	where
		logRefereeActions` :: ![RefereeAction] -> String
		logRefereeActions` []			= ""
		logRefereeActions` [ref:refls]	= "\n\t" <+++ logRefereeAction ref <+++ logRefereeActions` refls
		
		logRefereeAction :: !RefereeAction -> String
		logRefereeAction (ReprimandPlayer   tfp r)	= "(ReprimandPlayer " <+++ tfp.playerNr <+++ " " <+++ r <+++ ")"
		logRefereeAction (Hands 			tfp)	= "(Hands "           <+++ tfp.playerNr <+++ ")"
		logRefereeAction (TackleDetected	tfp)	= "(TackleDetected "  <+++ tfp.playerNr <+++ ")"
		logRefereeAction (DangerousPlay		tfp)	= "(DangerousPlay "   <+++ tfp.playerNr <+++ ")"
		logRefereeAction  GameOver					= "GameOver"
		logRefereeAction (GameCancelled     mt)		= "(GameCancelled "   <+++ mt <+++ ")"
		logRefereeAction  PauseGame					= "PauseGame"
		logRefereeAction (AddTime 			t)		= "(AddTime "         <+++ t <+++ ")"
		logRefereeAction  EndHalf					= "EndHalf"
		logRefereeAction (Goal 				t)		= "(Goal "            <+++ t <+++ ")"
		logRefereeAction (Offside 			tfp)	= "(Offside "         <+++ tfp.playerNr <+++ ")"
		logRefereeAction (DirectFreeKick   	t p)	= "(DirectFreeKick "  <+++ t <+++ " " <+++ p <+++ ")"
		logRefereeAction (GoalKick         	t)		= "(GoalKick "        <+++ t <+++ ")"
		logRefereeAction (Corner	      	t e)	= "(Corner "          <+++ t <+++ " " <+++ e <+++ ")"
		logRefereeAction (ThrowIn          	t p)	= "(ThrowIn "         <+++ t <+++ " " <+++ p <+++ ")"
		logRefereeAction (Penalty		 	t)		= "(Penalty "         <+++ t <+++ ")"
		logRefereeAction (CenterKick		t)		= "(CenterKick "      <+++ t <+++ ")"
		logRefereeAction (Advantage			t)		= "(Advantage "       <+++ t <+++ ")"
		logRefereeAction (OwnBallIllegally  tfp)	= "(OwnBallIllegally" <+++ tfp.playerNr <+++ ")"
		logRefereeAction (DisplacePlayers	ds)		= "DisplacePlayers"
		logRefereeAction  ContinueGame				= "ContinueGame"
		logRefereeAction (TellMessage		txt)	= "(TellMessage "     <+++ txt <+++ ")"
