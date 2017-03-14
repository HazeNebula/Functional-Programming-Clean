implementation module matchGame

import StdEnvExt, fileIO
import guiInterface, matchControl, render
from   Parsers import parse, :: Parser, :: Result(..), :: SugPosition, :: Rose(..), :: RoseNode(..), :: SymbolTypes(..), 
                      :: SymbolType(..),
                      fail, yield, token, symbol, <&>, &>, <!>, <!+>, <!*>, number, digit
	
timeLeft					:: !FootballGame -> Bool
timeLeft game				= game.match.Match.playingTime > zero

defaultPlayingTime			:: PlayingTime
defaultPlayingTime			= minutes 1.0

incFrames					:: !FootballGame -> FootballGame
incFrames game=:{frames}	= {game & frames=frames+1}


instance zero Rank where
	zero					= { matchpoints = zero, goals_scored = zero, goals_against = zero }
instance == Rank where
	(==) r1 r2				= (r1.matchpoints,r1.goals_scored,r1.goals_against) == (r2.matchpoints,r2.goals_scored,r2.goals_against)
instance < Rank where
	(<) r1 r2				= r1.matchpoints <  r2.matchpoints || 
							  r1.matchpoints == r2.matchpoints && r1.goals_scored <  r2.goals_scored ||
							  r1.matchpoints == r2.matchpoints && r1.goals_scored == r2.goals_scored && r1.goals_against > r2.goals_against
instance + Rank where
	(+) r1 r2				= { matchpoints   = r1.matchpoints   + r2.matchpoints
							  , goals_scored  = r1.goals_scored  + r2.goals_scored
							  , goals_against = r1.goals_against + r2.goals_against
							  }

competition					:: ![Home FootballField -> Team] !FootballField !Referee !PlayingTime !RandomSeed -> Competition
competition teams field referee playingtime rs
	= { results				= [ [  if (nr_west == nr_east) 
							          Nothing 
							          (Just (computeMatch (setMatchStart (team_west West field) (team_east East field) field referee playingtime rs)))
							    \\ (nr_east,team_east) <- zip2 [1..] teams
							    ]
							  \\ (nr_west,team_west) <- zip2 [1..] teams
							  ]
	  , west				= map (\f -> nameOf (f West field)) teams
	  , east				= map (\f -> nameOf (f East field)) teams
	  , usedRandomSeed		= rs
	  }

computeMatch				:: !Match -> Score
computeMatch match
| match.Match.playingTime > zero
							= computeMatch (snd (stepMatch match))
| otherwise					= match.score

ranking						:: ![ClubName] ![Maybe Score] -> Ranking
ranking names scores		= foldl upd [(t,zero) \\ t <- names] (zip2 [(tw,te) \\ tw <- names, te <- names] scores)
where	
	upd ranking (_,Nothing)
		= ranking
	upd ranking ((west,east),Just (goals_west,goals_east))
		= updkeyvalue west ((+) rank_west) (updkeyvalue east ((+) rank_east) ranking)
	where
		(mps_west, mps_east)	= if (goals_west > goals_east) (3,0) (if (goals_west < goals_east) (0,3) (1,1))
		(rank_west,rank_east)	= ({matchpoints=mps_west,goals_scored=goals_west,goals_against=goals_east}
								  ,{matchpoints=mps_east,goals_scored=goals_east,goals_against=goals_west}
								  )

instance toString Options where
	toString {closeReferee,showSplash,displaySpeed,showReferee,playingTime,renderStyle}
							= "{closeReferee=" <+++ closeReferee       <+++
							  ",showSplash="   <+++ showSplash         <+++
							  ",displaySpeed=" <+++ displaySpeed       <+++
							  ",showReferee="  <+++ showReferee        <+++
							  ",playingTime="  <+++ playingTime        <+++
							  ",renderStyle="  <+++ nameOf renderStyle <+++
							  "}"
instance fromString Options where
	fromString str
	= case parse optionsP (fromString str) optionsFile "char" of
		Succ [opt:_]		= opt
		_					= defaultOptions
	where
		optionsP			:: Parser Char Options Options
		optionsP			= token ['{closeReferee=']  &>
							  boolP                    <&> \closeReferee ->
							  token [',showSplash=']    &>
							  boolP                    <&> \showSplash   ->
							  token [',displaySpeed=']  &> 
							  displaySpeedP            <&> \displaySpeed ->
							  token [',showReferee=']   &>
							  boolP                    <&> \showReferee  ->
							  token [',playingTime=']   &>
							  timeP                    <&> \playingTime  ->
							  token [',renderStyle=']   &>
							  renderP                  <&> \renderStyle  ->
							  symbol '}'                &>
							  yield { closeReferee = closeReferee
							        , showSplash   = showSplash
							        , displaySpeed = displaySpeed
							        , showReferee  = showReferee
							        , playingTime  = playingTime
							        , renderStyle  = renderStyle
							        }
		boolP				= (token ['True'] &> yield True) <!> (token ['False'] &> yield False)
		renderP				= firstP [token (fromString (nameOf style)) &> yield style \\ style <- allRenderStyles] <!> yield (hd allRenderStyles)
		firstP []			= fail
		firstP [p:ps]		= p <!> firstP ps
		timeP				= <!+> digit        <&> \mts   ->
							      (symbol ':')  <&> \colon ->
							  <!+> digit        <&> \secs  ->
							  (token [' min'])  <&> \_     ->
							  yield (minutes ((toReal (toInt (toString mts))) + (toReal (toInt (toString secs)))/60.0))
		displaySpeedP		= (token ['Slowest'] &> yield Slowest) <!> 
							  (token ['Slower']  &> yield Slower)  <!>
							  (token ['Normal']  &> yield Normal)  <!>
							  (token ['Faster']  &> yield Faster)  <!>
							  (token ['Fastest'] &> yield Fastest)

instance == Options where
	(==) o1 o2				= o1.closeReferee                  == o2.closeReferee                 &&
							  o1.showSplash                    == o2.showSplash                   &&
							  o1.displaySpeed                  == o2.displaySpeed                 &&
							  o1.showReferee                   == o2.showReferee                  &&
							  o1.Options.playingTime           == o2.Options.playingTime          &&
							  o1.renderStyle.RenderStyle.name  == o2.renderStyle.RenderStyle.name
	
getOptions					:: !*env -> (!Options,!*env)	| FileSystem env
getOptions env
	= case readFile optionsFile env of
		(Just options,env)	= (fromString options,env)
		(nothing,     env)	= (defaultOptions,    env)

setOptions					:: !Options !*env -> *env		| FileSystem env
setOptions options env		= writeFile False optionsFile (toString options) env

optionsFile					:== "SoccerFun_options.txt"

defaultOptions				:: Options
defaultOptions
	= { closeReferee		= True
	  , showSplash			= False
	  , displaySpeed		= Normal
	  , showReferee			= True
	  , playingTime			= defaultPlayingTime
	  , renderStyle			= hd allRenderStyles
	  }

checkCompetitionFile		:: ![ClubName] !RandomSeed !*env -> (!(!RandomSeed,![Maybe Score]),!*env) | FileSystem env
checkCompetitionFile west rs env
# (ok,cf,env)				= fopen competitionFile FReadText env
| not ok					= ((rs,[]), createCompetitionFile west rs env)						// competition file does not exist: create it
# (ok,frs,fwest,cf)			= header cf
| not ok || fwest <> teams_line west
							= ((rs,[]), createCompetitionFile west rs (snd (fclose cf env)))	// competition file ill-formatted or different set of teams: create it
# (scores,cf)				= readScores cf														// competition file exists, and for this competition
# (ok,env)					= fclose cf env
| not ok					= abort ("Could not close competition file after reading scores.\n" <+++ length scores)
| otherwise					= ((frs,scores),env)
where
	readScores				:: !*File -> (![Maybe Score],!*File)
	readScores cf
	# (end,cf)				= fend cf
	| end					= ([],cf)
	# (line,cf)				= freadline cf
	# score					= if (line.[0] == 'x') Nothing
							 (let (i1,l1)	= span ((<>) ' ') [c \\ c<-:line]
							      (i2,l2)	= span ((<>) ' ') (tl l1)
							   in Just (toInt (toString i1),toInt (toString i2))
							 )
	# (scores,cf)			= readScores cf
	= ([score:scores],cf)

appendMatchToCompetitionFile:: !ClubName !ClubName !*env -> (!Int,!*env) | FileSystem env
appendMatchToCompetitionFile west east env
# (ok,cf,env)				= fopen competitionFile FAppendText env
| not ok					= abort "Could not open competition file for appending data.\n"
# (pos,cf)					= fposition cf
# (ok,env)					= fclose (cf <<< "x " <<< west <<< " vs " <<< east <<< '\n') env
| not ok					= abort "Could not close competition file after appending data.\n"
| otherwise					= (pos,env)

updateMatchToCompetitionFile:: !ClubName !ClubName !(Maybe Score) !Int !*env -> *env | FileSystem env
updateMatchToCompetitionFile west east score pos env
# (ok,cf,env)				= fopen competitionFile FAppendText env
| not ok					= abort "Could not open competition file for appending data.\n"
# (ok,cf)					= fseek cf pos FSeekSet
| not ok					= abort "Could not seek in competition file for updating data.\n"
# (ok,env)					= fclose (cf <<< result <<< ' ' <<< west <<< " vs " <<< east <<< '\n') env
| not ok					= abort "Could not close competition file after appending data.\n"
| otherwise					= env
where
	result					= case score of
								Nothing      = "x"
								Just (gw,ge) = gw +++> (" " <+++ ge)

createCompetitionFile		:: ![ClubName] !RandomSeed !*env -> *env | FileSystem env
createCompetitionFile west rs env
# (ok,cf,env)				= fopen competitionFile FWriteText env
| not ok					= abort "Could not create competition file.\n"
# (ok,env)					= fclose (cf <<< seed_line rs <<< '\n' <<< teams_line west <<< '\n') env
| not ok					= abort "Could not close competition file.\n"
| otherwise					= env

header						:: !*File -> (!Bool,!RandomSeed,!String,!*File)
header file
# (rs_line,   file)			= freadline file
# (teams_line,file)			= freadline file
= (size rs_line > 1 && size teams_line > 1, fromString (rs_line%(0,size rs_line-2)), teams_line%(0,size teams_line-2),file)

seed_line					:: !RandomSeed -> String
seed_line rs				= toString rs

teams_line					:: ![ClubName] -> String
teams_line west				= foldl (\t ts -> t +++ "," +++ ts) "" west

competitionFile				:== "competition.txt"
