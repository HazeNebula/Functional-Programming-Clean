definition module Team

/** This module defines the Soccer-Fun API that is concerned with teams.
	All available teams are collected in this module (allAvailableTeams).
*/
import Footballer

allAvailableTeams	:: [Home FootballField -> Team]

::	Team			:== [Footballer]		// the fielders are supposed to have different numbers, and all not equal to 1

instance nameOf   	Team

validateTeam		:: !Team -> Team
isValidTeam			:: !Team -> Bool
allPlayersAtHome	:: !FootballField !Home !Team -> Bool
replaceInTeam		:: ![Footballer] !Team -> Team
getTeam				:: !ClubName ![Team] -> Team

class mirror a		:: !FootballField !a -> a
instance mirror [a] | mirror a
instance mirror Footballer
instance mirror Position
instance mirror Speed
instance mirror Angle
