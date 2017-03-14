definition module guiInterface

import matchGame

/**	getSplashImageForGui loads the splash image for Soccer-Fun.
*/
getSplashImageForGui	:: !*env -> (!GamePicture,!*env) | FileSystem env

//	todo:
getActionPicsForGui		:: !Match !*env -> (!ActionPics,!*env) | FileSystem env

/*	Steps the game one step, returns the game and the referee events
*/
stepMatchForGui			:: !FootballGame !*env -> (![RefereeAction], !FootballGame, !*env) | FileSystem env

:: DisplaySpeed			= Slowest | Slower | Normal | Fast | Faster | Fastest
instance toString		DisplaySpeed
instance fromString		DisplaySpeed
instance ==				DisplaySpeed

intervalFactor			:: !DisplaySpeed -> Real
