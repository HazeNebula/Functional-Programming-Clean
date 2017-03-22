implementation module Team

import StdEnvExt
import Footballer
/*	Import all standard teams: */
import TeamMiniEffie
import Team_Opponent_Slalom_Assignment
import Team_Opponent_Passing_Assignment
import Team_Opponent_DeepPass_Assignment
import Team_Opponent_Keeper_Assignment
import Team_Student_Rounds_Assignment
import Team_Student_Slalom_Assignment
import Team_Student_Passing_Assignment
import Team_Student_DeepPass_Assignment
import Team_Student_Keeper_Assignment
// Marc Schoolderman's team:
import Team_Harmless
import TeamBoolshit
import TeamGoalkeeper
import TeamDefender
import TeamVanguard
import TeamBoolshit2

allAvailableTeams					:: [Home FootballField -> Team]
allAvailableTeams					= [ Team_MiniEffies, Harmless, Team_Boolshit, Team_Goalkeeper, Team_Defender, Team_Vanguard, Team_Boolshit2
									  , Team_Student_Rounds
									  , Team_Student_Slalom
									  , Team_Student_Passing
									  , Team_Student_DeepPass
									  , Team_Student_Keeper
									  , Team_Opponent_Slalom
									  , Team_Opponent_Passing
									  , Team_Opponent_DeepPass
									  , Team_Opponent_Keeper
									  ]

instance nameOf Team where	nameOf players
									= case players of
										[fb:_]		= nameOf fb.playerID
										none		= abort "nameOf[Team]: applied to empty team.\n"

validateTeam						:: !Team -> Team
validateTeam team					= map validateFootballer team
where
	validateFootballer				:: !Footballer -> Footballer
	validateFootballer fb=:{length}	= {fb & length  = setbetween length min_length max_length
									      , stamina = max_stamina
									      , health  = max_health
									  }

isValidTeam :: !Team -> Bool
isValidTeam team					= length clubNames == 1
										&&
									  (isEmpty keepers || isValidKeeper (hd keepers))
										&&
									  all isValidPlayer players
									    &&
									  sort (map nrOf players) == sort (removeDup (map nrOf players))
									    &&
									  not (isMember 1 (map nrOf fielders))
where
	(keepers,fielders)				= spanfilter isKeeper team
	clubNames						= removeDup (map clubOf players)
	clubName						= hd clubNames
	players							= keepers ++ fielders
	clubOf fb						= fb.playerID.clubName
	nrOf   fb						= fb.playerID.playerNr
	isValidKeeper fb				= fb.playerID == {clubName=clubName,playerNr=1}
	isValidPlayer fb				= clubOf fb == clubName

allPlayersAtHome					:: !FootballField !Home !Team -> Bool
allPlayersAtHome field home team	= all atHome team
where
	atHome							= if (home == West) (\player -> player.Footballer.pos.px <= zero)
						                                (\player -> player.Footballer.pos.px >= zero)

replaceInTeam						:: ![Footballer] !Team -> Team
replaceInTeam fbs team				= removeMembers team fbs ++ fbs

getTeam								:: !ClubName ![Team] -> Team
getTeam cn teams					= case [team \\ team<-teams | nameOf team==cn] of
										[team:_]				= team
										_						= abort ("Team " <+++ cn <+++ " does not seem to exist.\n")

instance mirror [a] | mirror a		where mirror field as		= map (mirror field) as
instance mirror Footballer			where mirror field fb		= {fb & pos   = mirror field fb.pos
		                                                              , nose  = mirror field fb.nose
		                                                              , speed = mirror field fb.speed
		                                                          }
instance mirror Position			where mirror field pos		= {pos & px = ~pos.px}
instance mirror Speed				where mirror field speed	= {speed & direction = mirror field speed.direction}
instance mirror Angle				where mirror field angle	= rad pi - angle
