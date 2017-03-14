definition module matchLog

/**	This module defines the logging facilities of Soccer-Fun.
*/
import matchControl

/** Logging a match.
*/
::	WhatToLog	= { footballerActions	:: !Bool
				  , fbPositions			:: !Bool
				  , refEvents			:: !Bool
				  , ballPosition		:: !Bool
				  }
instance toString WhatToLog

getWhatToLog	:: !*env -> (!WhatToLog,!*env) | FileSystem env

logFile			:: String

/*	logMatch options match refereeActions succeededActions env
		writes the indicated log fields to logFile.
*/
logMatch		:: !WhatToLog !Match ![RefereeAction] !(AssocList FootballerID FootballerAction) !*env -> *env | FileSystem env
