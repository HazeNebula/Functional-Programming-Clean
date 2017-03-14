implementation module Gui2D

import StdEnvExt, StdIOExt
import fileIO
import digitdisplay, textdisplay, render, matchGame, RangeSlider
import StdDebug

SoccerFunGUI2D						:: !*World -> *World
SoccerFunGUI2D world
# (theGame, world)		 			= getBeginMatch world
# (ids,     world)					= openIds 4 world
# (displIds,world)					= sseqList (repeatn 2 openDigitDisplayId) world
# (teamdIds,world)					= sseqList (repeatn 5 openTextDisplayId)  world
# (frameId, world)					= openTextDisplayId world
= startIO SDI theGame (initGUI (ids,displIds,teamdIds,frameId)) [ProcessClose closeProcess] world
where
	getBeginMatch					:: !*env -> (!FootballGame,!*env) | TimeEnv, FileSystem env
	getBeginMatch env
	| isEmpty teams					= abort "There are no teams.\nPlease add teams to \"allAvailableTeams\" in the \"Team\"-module\n"	
	| isEmpty referees				= abort "There are no referees.\nPlease add referees to \"allAvailableReferees\" in the \"Referee\"-module\n"
	# (whatToLog,env)				= getWhatToLog     env
	# (options,  env)				= getOptions       env
	# (seed,     env)				= getNewRandomSeed env
	# match							= setMatchStart (hd teams West field) (hd teams East field) field (hd referees field) options.Options.playingTime seed
	= ( { match						= match
		, actionPics				= {refereePics=[]}
		, history					= {time=s 2.0,past=[]}
		, frames					= 0
		, options					= options
		, logging					= whatToLog
		}
	  , env
	  )
	where
		teams						= allAvailableTeams
		referees					= allAvailableReferees
		field						= getDefaultField

	initGUI							:: !(![Id],![DigitDisplayId],![TextDisplayId],!TextDisplayId) !(PSt FootballGame) -> (PSt FootballGame)	
	initGUI ([timerId,fieldId,windowId,splashId],[westId,eastId],[team1Id,team2Id,halfId,timeId,refId],framesId) pSt=:{ls=ls=:{match=matchAtStart,options}}
	# (penAtts,  pSt)				= accPIO renderAttributes pSt
	# (penFont,  pSt)				= accPIO (accScreenPicture (openFont {fName="Courier New",fSize=32,fStyles=[BoldStyle]})) pSt
	# (frameFont,pSt)				= accPIO (accScreenPicture (openFont {fName="Courier New",fSize=16,fStyles=[BoldStyle]})) pSt
	# (underFont,pSt)				= accPIO (accScreenPicture (openFont {fName="Courier New",fSize=12,fStyles=[BoldStyle]})) pSt
	# refsName						= nameOf matchAtStart.Match.theReferee
	# wDef							= Window "SoccerFun: Van Gaal's Electronic Notebook" 
										(	LayoutControl
											(	    teamName team1Id (nameOf matchAtStart.Match.team1) (snd penFont) WestColour
												:+: DigitDisplay  westId (IntegerFormat 2) digitDisplaySize WestColour []
												:+: DigitDisplay  eastId (IntegerFormat 2) digitDisplaySize EastColour []
												:+: teamName team2Id (nameOf matchAtStart.Match.team2) (snd penFont) EastColour
											)	[ ControlPos (Center,zero) ]
											:+: fpsDisplay framesId frameRatePrefix (snd frameFont) Green 
											:+:	CustomControl footballFieldDisplaySize (options.renderStyle.look ls.match)
													[ ControlId     fieldId
													, ControlPen    penAtts
													, ControlPos    (Center,zero)
													, ControlResize (\_ _ wViewSize=:{h} -> {wViewSize & h=h-digitDisplaySize.h-60})
													]
											:+: TextDisplay halfId (showHalf matchAtStart.Match.playingHalf) {w=80,h=15}
													[ ControlPen [PenColour White,PenBack Black,PenFont (snd underFont)]
													//, ControlResize (\_ _ size -> {size & h=size.h-digitDisplaySize.h})
													, ControlPos (LeftBottom,OffsetVector {vx=10,vy=(~7)})
													]
											:+: TextDisplay timeId (toString matchAtStart.Match.playingTime) {w=100,h=15}
													[ ControlPen [PenColour White,PenBack Black,PenFont (snd underFont)]
													, ControlPos (Center,OffsetVector {vx=zero,vy=zero})
													]
											:+: TextDisplay refId refsName {w=80+(12*(size refsName)),h=15}
													[ ControlPen [PenColour White,PenBack Black,PenFont (snd underFont)]
													, ControlPos (RightBottom,OffsetVector {vx=zero,vy=(~7)})//(3*(size "test"))
													]
										)
										[ WindowViewSize	{w=footballFieldDisplaySize.w-1, h=footballFieldDisplaySize.h+digitDisplaySize.h}
										, WindowLook False	stdUnfillNewFrameLook
										, WindowPen			[PenBack Black]
										, WindowId			windowId
										]
	# (error,pSt)					= openWindow undef wDef pSt
	# pSt							= setDigitDisplayValue westId (fst ls.match.score) pSt
	# pSt							= setDigitDisplayValue eastId (snd ls.match.score) pSt
	| error<>NoError				= abort "Could not create window.\n"
	# speed							= intervalFactor ls.options.displaySpeed
	# tDef							= Timer (toInt ((toReal ticksPerSecond)*(toReal matchAtStart.Match.unittime)*speed)) NilLS 
										[ TimerFunction    (noLS1 (const (nextstep timerId fieldId westId eastId)))
                    	    		    , TimerId          timerId
                        			    , TimerSelectState Unable
                            			]
	# (error,pSt)					= openTimer undef tDef pSt
	| error<>NoError				= abort "Could not create step timer.\n"
	# tDef							= Timer ticksPerSecond NilLS        [TimerFunction (noLS1 (const (frameRatef timeId halfId framesId)))]
	# (error,pSt)					= openTimer undef tDef pSt
	| error<>NoError				= abort "Could not open frame rate timer.\n"
	# mDef							= Menu "&File"
										(	MenuItem "E&xit"            [MenuShortKey 'q',MenuFunction (noLS quitf)]
										)	[]
	# (error,pSt)					= openMenu undef mDef pSt
	| error<>NoError				= abort "Could not create File menu.\n"
	# mDef2							= Menu "&Game"
										(   MenuItem "&Match"           [MenuShortKey 'm',MenuFunction (noLS (matchDialogf westId eastId fieldId timerId refId team1Id team2Id))]
										:+: MenuItem "Competition"      [MenuShortKey 'u',MenuFunction (noLS (competitionDialogf timerId refId))]
										:+: MenuSeparator               []
										:+: MenuItem "&Run"             [MenuShortKey 'r',MenuFunction (noLS (continuef timerId))]
										:+:	MenuItem "&Step"            [MenuShortKey 's',MenuFunction (noLS (stepf timerId fieldId westId eastId))]
										:+:	MenuItem "&Halt"            [MenuShortKey 'h',MenuFunction (noLS (haltf timerId))]
										:+: MenuSeparator               []
										:+: SubMenu "&Mode"             ( RadioMenu [ ("&Realistic",  Nothing,Nothing,noLS realisticf)
										                                            , ("&Predictable",Nothing,Nothing,noLS predictablef)
										                                            ] 0 []
										                                ) []
										:+: SubMenu "Sp&eed"            ( RadioMenu [ ("&Slowest",Nothing, Just '`', noLS (changeSpeedf timerId Slowest))
											                                        , ("S&lower", Nothing, Just '1', noLS (changeSpeedf timerId Slower))
											                                        , ("N&ormal", Nothing, Just '2', noLS (changeSpeedf timerId Normal))
											                                        , ("F&ast",   Nothing, Just '3', noLS (changeSpeedf timerId Fast))
											                                        , ("&Faster", Nothing, Just '4', noLS (changeSpeedf timerId Faster))
											                                        , ("Fas&test",Nothing, Just '5', noLS (changeSpeedf timerId Fastest))
											                                        ] ([Slowest,Slower,Normal,Faster,Fastest]??options.displaySpeed+1) []
											                            ) []
										:+: SubMenu "Re&feree"          ( RadioMenu [ ("&Show",   Nothing, Just '+', noLS (showReff True))
											                                        , ("&NoShow", Nothing, Just '-', noLS (showReff False))
											                                        ] ([True,False]??options.showReferee+1) []
															            ) []
										:+: SubMenu "R&ender"           ( RadioMenu [ (nameOf style, Nothing, Nothing, noLS (setfieldlook fieldId style))
										                                            \\ style <- allRenderStyles 
										                                            ] ((map nameOf allRenderStyles)??(nameOf options.renderStyle)+1) []
										                                ) []
										:+: MenuItem "&Playing Time..." [MenuFunction (noLS (playtimef westId eastId fieldId timeId)), MenuShortKey 't']
										:+: MenuItem "Splash screen"    [MenuFunction (noLS (setSplashScreenf splashId)),MenuMarkState (if options.showSplash Mark NoMark),MenuId splashId]
										)	[]
	# (error,pSt)					= openMenu undef mDef2 pSt
	| error<>NoError				= abort "Could not create Game menu.\n"
	# pSt							= appPIO (setWindowViewSize windowId {w=640,h=550}) pSt
	# pSt							= showSplashScreen pSt
	= pSt

	showSplashScreen				:: !(PSt FootballGame) -> PSt FootballGame
	showSplashScreen pSt=:{ls=game}
	| not game.options.showSplash	= pSt
	# (spls,pSt)					= getSplashImageForGui pSt
	# splashDef						= Dialog "The Footballer's Mind"
										(	CompoundControl 
											(	ButtonControl "&Close" [ControlFunction (noLS closeActiveWindow)]
											)
											[	ControlLook True (const2 (drawAt zero spls.img))
											,	ControlViewSize  (getBitmapSize spls.img)
											]
										) []
	# ((error,_),pSt)				= openModalDialog undef splashDef pSt
	| error<>NoError				= abort "Could not open splash screen.\n"
	| otherwise						= pSt


//	Constants for the GUI:
digitDisplaySize					= { w=24, h=36 }
footballFieldDisplaySize			= { w=640,h=400}
frameRatePrefix						:== "Rounds/sec: "

//	GUI elements:

/** teamName id name font colour
		describes a TextDisplay that is identified by id, has text content name, uses the given font in the given colour.
*/
teamName							:: TextDisplayId String Font Colour -> TextDisplay .ls .pst
teamName id name penFont colour		= TextDisplay id name (teamSize footballFieldDisplaySize)
										[ ControlPen [PenColour colour,PenBack Black,PenFont penFont]
										, ControlResize (const2 teamSize)
										]
where teamSize {w}					= {digitDisplaySize & w=(w-digitDisplaySize.w*4)/2}

/** fpsDisplay id name font colour
		describes a TextDisplay that is used to display the frame rate of a match.
*/
fpsDisplay							:: TextDisplayId String Font Colour -> TextDisplay .ls .pst
fpsDisplay id name penFont colour	= TextDisplay id name {digitDisplaySize & w=300} 
										[ ControlPen [PenColour colour, PenBack Black, PenFont penFont]
								        , ControlPos (Center,zero)
										]


//	The callback functions of the GUI:

/** setSplashScreenf splashId pSt
		sets the check mark of the menu item that controls the splash screen option, and updates the Options record in the state accordingly.
*/
setSplashScreenf					:: !Id !(PSt FootballGame) -> PSt FootballGame
setSplashScreenf splashId pSt=:{ls=game}
| game.options.showSplash			= appPIO (unmarkMenuItems [splashId]) {pSt & ls={game & options={game.options & showSplash = False}}}
| otherwise							= appPIO (markMenuItems   [splashId]) {pSt & ls={game & options={game.options & showSplash = True}}}

/** quitf pSt
		stores the current options to disk and terminates the parent interactive process.
*/
quitf								:: !(PSt FootballGame) -> PSt FootballGame
quitf pSt=:{ls=game}
	# pSt							= setOptions game.options pSt
	# pSt							= closeProcess pSt
	= pSt

/** continuef timerId pSt
		continues the simulation of the current match.
*/
continuef							:: !Id !(PSt FootballGame) -> PSt FootballGame
continuef timerId pSt				= appPIO (enableTimer timerId) pSt

/** haltf timerId pSt
		stops the simulation of the current match.
*/
haltf								:: !Id !(PSt FootballGame) -> PSt FootballGame
haltf timerId pSt					= appPIO (disableTimer timerId) pSt

/** stepf timerId fieldId westId eastId pSt
		stops the simulation of the current match, and computes a single step of the current match.
*/
stepf :: !Id !Id !DigitDisplayId !DigitDisplayId -> IdFun (PSt FootballGame)
stepf timerId fieldId westId eastId	= nextstep timerId fieldId westId eastId o haltf timerId

/** nextstep timerId fieldId westId eastId pSt
		computes a single step for the current match, renders the new state of the match, and displays the referee dialog if required.
*/
nextstep							:: !Id !Id !DigitDisplayId !DigitDisplayId !(PSt FootballGame) -> PSt FootballGame
nextstep timerId fieldId westId eastId pSt=:{ls=game,io}
	# (refEvents,game,io)			= stepMatchForGui game io
	# pSt							= {pSt & io = setControlLook fieldId True (False,game.options.renderStyle.look game.match) io
									       , ls = incFrames game
									  }
	= analyseRefEvents refEvents pSt
where
	analyseRefEvents				:: ![RefereeAction] !(PSt FootballGame) -> PSt FootballGame
	analyseRefEvents refEvents pSt	= foldl analyseRefEvent pSt refEvents
	where
		analyseRefEvent :: !(PSt FootballGame) !RefereeAction -> PSt FootballGame
		analyseRefEvent pSt=:{ls=ls=:{FootballGame | match={score,playingHalf}}} rev
		| or (apply rev [isContinueGame,isDisplacePlayers,isDirectFreeKick,isCenterKick,isPauseGame,isAddTime])
									= pSt
		# pSt						= refereeDialog rev pSt
		# pSt						= if (isGameOver rev) (appPIO (disableTimer timerId) pSt) pSt
		= case rev of
			Goal h					= if (h == West && playingHalf == FirstHalf || h == East && playingHalf == SecondHalf)
									     (setDigitDisplayValue westId (fst score) pSt)
									     (setDigitDisplayValue eastId (snd score) pSt)
			GameCancelled _			= setDigitDisplayValue westId (fst score) (setDigitDisplayValue eastId (snd score) pSt)
			no_goal					= pSt
		
		refereeDialog				:: !RefereeAction !(PSt FootballGame) -> PSt FootballGame
		refereeDialog rev pSt=:{ls=ls=:{match=match=:{theReferee=referee=:{Referee | name}}}}
		| not pSt.ls.options.showReferee
									= pSt
		# pSt						= case defaultSoundFile rev of
										Just sound	= appPIO (makeSound sound) pSt
										nothing		= pSt
		# pSt						= haltf timerId pSt
		# (image,pSt)				= accPIO (defaultImage match rev) pSt
		# (closeId, pSt)			= openId pSt
		# (dialogId,pSt)			= openId pSt
		# tDef						= Timer (5*ticksPerSecond) NilLS [ TimerFunction (noLS1 (const ((continuef timerId) o (closeWindow dialogId) o (appPIO (closeTimer closeId)))))
									                                 , TimerId       closeId
									                                 ]
		# (error,  pSt)				= openTimer undef tDef pSt
		| error<>NoError			= abort "Could not open referee dialog timer.\n"
		# refereeDef				= Dialog ("Referee " <+++ name)
										(	TextControl (showSuccintRefereeAction rev) [ControlWidth (PixelWidth 200)]
										:+: CustomControl (getBitmapSize image) (const2 (drawAt zero image)) [ControlPos (BelowPrev,zero)]
										)	[WindowId dialogId]
		# ((error,_),pSt)			= openModalDialog undef refereeDef pSt
		| error<>NoError			= abort "Could not open referee dialog.\n"
		| otherwise					= pSt

/** showReff show pSt
		sets the option to show the referee during simulation.
*/
showReff							:: !Bool !(PSt FootballGame) -> PSt FootballGame
showReff show pSt					= {pSt & ls = {pSt.ls & options = {pSt.ls.options & showReferee = show}}}

/** playtimef westId eastId fieldId timeId pSt
		opens the dialog to alter the play time of a match.
*/
playtimef							:: !DigitDisplayId !DigitDisplayId !Id !TextDisplayId !(PSt FootballGame) -> PSt FootballGame
playtimef westId eastId fieldId timeId pSt=:{ls=game=:{options={Options | playingTime}}}
	# (dialogId,pSt)				= openId pSt
	# (textId,  pSt)				= openId pSt
	# (sliderId,pSt)				= openRangeSliderId pSt
	# playingtimeDef				= Dialog "Playing Time"
										(   TextControl (toString playingTime) [ControlId textId, ControlPos (Center,zero),ControlWidth (ContentWidth (toString (maxList times)))]
										:+: RangeSlider sliderId Horizontal (PixelWidth 16) {values=times,index=times??playingTime} (noLS1 (setPlayingTime westId eastId textId)) []
										:+: ButtonControl "Close" [ControlFunction (noLS (closeWindow dialogId)),ControlPos (Right,zero)]
										)
										[ WindowClose (noLS (closeWindow dialogId))
										, WindowId dialogId
										]
	# ((error,_),pSt)				= openModalDialog undef playingtimeDef pSt
	| error <> NoError				= abort "Could not open Playing Time dialog.\n"
	| otherwise						= pSt
where
	times							= map minutes [0.5, 1.0 .. 10.0]
	
	setPlayingTime westId eastId textId newtime pSt=:{ls=game}
		# pSt						= setTextDisplayText timeId display_time pSt
		# pSt						= appPIO (setControlText textId display_time) pSt
		# pSt						= {pSt & ls = {game & options = {Options | game.options & playingTime=newtime}
									                    , match   = {Match   | game.match   & playingTime=newtime}
									  }           }
		# pSt						= restartf westId eastId fieldId pSt
		= pSt
	where
		display_time				= toString newtime

/** changeteamf fieldId team1Id team2Id home team pSt
		replaces the current team at home with the given team. The display name of the team is adapted, and the match is to the beginning of the first half.
*/
changeteamf							:: !DigitDisplayId !DigitDisplayId !Id !TextDisplayId !TextDisplayId !Home !Team !(PSt FootballGame) -> PSt FootballGame
changeteamf westId eastId fieldId team1Id team2Id home team pSt=:{ls=game=:{match,options},io}
	# match							= {Match | match & team1 = if (home == West) team match.Match.team1, team2 = if (home == East) team match.Match.team2}
	# pSt							= {pSt   & ls = {game & match=match}}
	# pSt							= setTextDisplayText (if (home == West) team1Id team2Id) (nameOf team) pSt
	# pSt							= restartf westId eastId fieldId pSt
	= pSt

/** changereff timerId refId referee pSt
		replaces the current referee with the given referee and adapts the display name of the new referee.
*/
changereff							:: !Id !TextDisplayId !Referee !(PSt FootballGame) -> PSt FootballGame
changereff timerId refId ref=:{Referee | name} pSt
	# pSt							= setTextDisplayText refId name pSt //todo: de positionering moet aangepast worden 
	= appPIO (disableTimer timerId) {pSt & ls={pSt.ls & match = {pSt.ls.match & theReferee = ref}}}

/** restartf fieldId pSt
		makes sure that the current match is reinitialized to the initial teams, referee, and ball position.s
*/
restartf							:: !DigitDisplayId !DigitDisplayId !Id !(PSt FootballGame) -> PSt FootballGame
restartf westId eastId fieldId pSt=:{ls=game=:{match,options},io}
	# match							= {match & theBall     = Free zero
									         , playingHalf = FirstHalf
									         , playingTime = options.Options.playingTime
									         , score       = (0,0)
									         , theReferee  = getRefereeFresh   match.Match.theField (nameOf match.Match.theReferee)
									         , team1       = getTeamFresh West match.Match.theField (nameOf match.Match.team1)
									         , team2       = getTeamFresh East match.Match.theField (nameOf match.Match.team2)
									  }
	# pSt							= setDigitDisplayValue westId 0 pSt
	# pSt							= setDigitDisplayValue eastId 0 pSt
	= {pSt & ls = {game & match=match}
	       , io = setControlLook fieldId True (False,options.renderStyle.look match) io
	  }
where
	getRefereeFresh					:: !FootballField !String -> Referee
	getRefereeFresh field name		= hd [r field \\ r <- allAvailableReferees | nameOf (r field) == name]
	
	getTeamFresh					:: !Home !FootballField !String -> Team
	getTeamFresh home field name	= hd [t \\ t <- getAllTeamsOfHome home field | nameOf t == name]

/**	setfieldlook fieldId style pSt
		sets a new rendering style for the football field
*/
setfieldlook						:: !Id !RenderStyle !(PSt FootballGame) -> PSt FootballGame
setfieldlook fieldId style=:{look} pSt=:{ls=game=:{match,options},io}
	= {pSt & ls = {game & options = {options & renderStyle = style}}
	       , io = setControlLook fieldId True (False,look match) io
	  }
	

/** changeSpeedf timerId speed pSt
		modifies the current simulation speed of a match.
*/
changeSpeedf						:: !Id !DisplaySpeed !(PSt FootballGame) -> PSt FootballGame
changeSpeedf timerId speed pSt=:{ls}
	# timerInterval 				= toInt ((toReal ticksPerSecond)*(toReal ls.match.Match.unittime)*(intervalFactor speed))
	# pSt							= {pSt & ls = {FootballGame | ls & options = {Options | ls.options & displaySpeed = speed}}}
	= appPIO (setTimerInterval timerId timerInterval) pSt

/** realisticf pSt
		sets a true pseudo-random generating function.
*/
realisticf							:: !(PSt FootballGame) -> PSt FootballGame
realisticf pSt=:{ls=ls=:{match}}	= {pSt & ls={FootballGame | ls & match={match & nextRandomP=nextRandomP}}}

/** predictablef pSt
		sets a 'random-generating' function that always yields 1.0.
*/
predictablef						:: !(PSt FootballGame) -> PSt FootballGame
predictablef pSt=:{ls=ls=:{match}}	= {pSt & ls={FootballGame | ls & match={match & nextRandomP=next1}}}

/** frameRatef timeId halfId framesId pSt
		updates the current playing time and playing half information, as well as the frame rate counter, which is reset every second.
*/
frameRatef							:: !TextDisplayId !TextDisplayId !TextDisplayId !(PSt FootballGame) -> PSt FootballGame
frameRatef timeId halfId framesId pSt=:{ls=ls=:{frames,match={Match | playingTime,playingHalf}}}
	# pSt							= setTextDisplayText timeId (toString playingTime) pSt
	# pSt							= setTextDisplayText halfId (showHalf playingHalf) pSt
	= setTextDisplayText framesId (frameRatePrefix <+++ frames) {pSt & ls={ls & frames=0}}

/** matchDialogf westId eastId fieldId timerId refId team1Id team2Id pSt
		opens a dialog in which the user can select two teams that play a match, and a referee to control the match.
*/
matchDialogf						:: DigitDisplayId DigitDisplayId Id Id TextDisplayId TextDisplayId TextDisplayId (PSt FootballGame) -> PSt FootballGame
matchDialogf westId eastId fieldId timerId refId team1Id team2Id pSt=:{ls=game}
	# (dialogId,pSt)				= openId pSt
	# dialog						= Dialog "Choose Match"
										(	LayoutControl
										    (   TextControl "Choose Team West" [ControlPos (Center,zero)]
										    :+: LayoutControl
										        (   RadioControl [  (nameOf t,Nothing,noLS (changeteamf westId eastId fieldId team1Id team2Id West t)) 
										                         \\ t <- teams West
										                         ] (Columns 1) ((map nameOf (teams West)) ?? (nameOf game.match.Match.team1)+1) []
										        ) [ControlPos (Center,zero)]
										    ) [ControlPos (Left,zero)]
										:+: LayoutControl
										    (   TextControl "Choose Team East" [ControlPos (Center,zero)]
										    :+: LayoutControl
										        (   RadioControl [  (nameOf t,Nothing,noLS (changeteamf westId eastId fieldId team1Id team2Id East t)) 
										                         \\ t <- teams East
										                         ] (Columns 1) ((map nameOf (teams East)) ?? (nameOf game.match.Match.team2)+1) []
										        ) [ControlPos (Center,zero)]
										    ) [ControlPos (RightToPrev,zero)]
										:+: LayoutControl
										    (   TextControl "Choose Referee" [ControlPos (Center,zero)]
										    :+: LayoutControl
										        (   RadioControl [  (nameOf r,Nothing,noLS (changereff timerId refId r)) 
										                         \\ r <- apply field allAvailableReferees
										                         ] (Columns 1) ((map nameOf (apply field allAvailableReferees)) ?? (nameOf game.match.theReferee)+1) []
										        ) [ControlPos (Center,zero)]
										    ) [ControlPos (RightToPrev,zero)]
										:+: ButtonControl "Ok" [ControlFunction (closef dialogId),ControlPos (Right,zero)]
										)
										[ WindowClose (closef dialogId)
										, WindowId    dialogId
										]
	# ((error,_),pSt)				= openModalDialog undef dialog pSt
	| error <> NoError				= abort "Could not open match dialog.\n"
	| otherwise						= pSt
where
	teams home						= getAllTeamsOfHome home field
	field							= game.match.theField
	closef dialogId					= noLS (restartf westId eastId fieldId o closeWindow dialogId)

/** competitionDialogf timerId refId pSt
		opens a dialog in which the user can select the teams that participate in a full competition; i.e. each team plays against each other at either home side of
		the football field. Once the teams and referee are selected, all matches are computed, and the final ranking is displayed.
*/
competitionDialogf					:: Id TextDisplayId (PSt FootballGame) -> PSt FootballGame
competitionDialogf timerId refId pSt=:{ls=game}
	# (dialogId,pSt)				= openId pSt
	# (teamsId, pSt)				= openId pSt
	# dialog						= Dialog "Competition"
										(	LayoutControl
											(	TextControl "Choose Teams West" [ControlPos (Center,zero)]
											:+: LayoutControl
												(	CheckControl [(nameOf t,Nothing,Mark,id) \\ t <- teams_west] (Columns 1) [ControlId teamsId]
										        ) [ControlPos (Center,zero)]
										    :+: ButtonControl "Select &All" 
										    		[ControlFunction (noLS (selectTeams teamsId (index_all_teams,index_no_teams))),ControlPos (Left,zero)]
										    :+: ButtonControl "Cl&ear All" 
										    		[ControlFunction (noLS (selectTeams teamsId (index_no_teams,index_all_teams))),ControlPos (RightToPrev,zero)]
										    ) [ControlPos (Left,zero)]
										:+: LayoutControl
										    (   TextControl "Choose Referee" [ControlPos (Center,zero)]
										    :+: LayoutControl
										        (   RadioControl [(nameOf r,Nothing,noLS (changereff timerId refId r)) \\ r <- apply field allAvailableReferees] (Columns 1) 1 []
										        ) [ControlPos (Center,zero)]
										    ) [ControlPos (RightToPrev,zero)]
										:+: ButtonControl "Ok" [ControlFunction (noLS (startCompetition teamsId dialogId)),ControlPos (Right,zero)]
										)
										[ WindowClose (noLS (closeWindow dialogId))
										, WindowId    dialogId
										, WindowPos   (Center,OffsetVector {zero & vy=100})
										]
	# ((error,_),pSt)				= openModalDialog undef dialog pSt
	| error <> NoError				= abort "Could not open competition dialog.\n"
	| otherwise						= pSt
where
	teams home						= getAllTeamsOfHome home field
	teams_west						= teams West
	index_all_teams					= [1 .. length teams_west]
	index_no_teams					= []
	field							= game.match.theField
	
	selectTeams checkId (set,clear)	= appPIO (markCheckControlItems checkId set o unmarkCheckControlItems checkId clear)
	
	startCompetition checkId dialogId pSt=:{ls=game}
	= case accPIO (getWindow dialogId) pSt of
		(Nothing, pSt)				= abort "Fatal error: could not retrieve competition dialog data.\n"	// should be impossible, because the dialog has not been closed yet
		(Just wSt,pSt=:{ls=game})
		= case getCheckControlSelection checkId wSt of
			(_,Nothing)				= closeWindow dialogId pSt
			(_,Just idxs)
				# (teams,names)		= unzip [(t,nameOf (t West game.match.theField)) \\ t <- allAvailableTeams & i <- [1..] | isMember i idxs]
				# ((rs,scores),pSt)	= checkCompetitionFile names game.match.seed pSt
				# compete			= competition teams game.match.theField game.match.theReferee game.match.Match.playingTime rs
				= showMatches dialogId compete scores {pSt & ls={game & match = {game.match & seed=rs}}}
	
	showMatches dialogId compete=:{west,east} scores pSt
		# pSt						= appPIO (closeAllControls dialogId) pSt
		# (textId,    pSt)			= openId pSt
		# (progressId,pSt)			= openId pSt
		# (matchId,   pSt)			= openId pSt
		# (error,     pSt)			= openControls dialogId undef 
										(   TextControl "" [ ControlWidth (ContentWidth (result (longest west) (longest east) (Just (99,99))))
										                   , ControlId    textId
										                   , ControlPos   (Center,OffsetVector {zero & vy=100})
										                   ]
										:+: TextControl "" [ ControlWidth (ContentWidth (progress nr_of_matches nr_of_matches))
										                   , ControlId    progressId
										                   , ControlPos   (BelowPrev,zero)
										                   ]
										) pSt
		# (error, pSt)				= openTimer ([(i,j) \\ i <- [0..length compete.west-1], j <- [0 .. length compete.east-1]],scores) 
									    (Timer 0 NilLS [TimerFunction (showNextMatch textId progressId matchId compete),TimerId matchId]) pSt
		| error <> NoError			= abort "Could not open timer to display matches.\n"
		| otherwise					= pSt
	where
		longest texts				= hd (sortBy (\t1 t2 -> size t1 > size t2) texts)
		nr_of_matches				= length west * length east
		
		result teamw teame score	= teamw +++ "-" +++ teame +++": " <+++ if (isNothing score) "no result" (toString (fromJust score))
		progress i total			= i +++> (" out of " <+++ total <+++ " (" <+++ toInt (100.0 * (toReal i / toReal total)) <+++ "%)")
		
		showNextMatch textId progressId matchId compete dt (([_:ms],[_:cs]),pSt)						// match has already been computed: skip it
			= showNextMatch textId progressId matchId compete dt ((ms,cs),pSt)
		showNextMatch textId progressId matchId compete=:{results,west,east} dt (([(tw,te):ms],cs),pSt)	// match has not yet been computed: compute and backup
			# (pos,pSt)				= appendMatchToCompetitionFile westt eastt pSt						// create an empty entry in the competition backup file
			# pSt					= updateMatchToCompetitionFile westt eastt score pos pSt			// after computing match, store in the competition backup file
			# pSt					= appPIO (setControlText textId (result westt eastt score) o		// show user which match was successfully computed
										setControlText progressId (progress (tw*length west + te) nr_of_matches)) pSt
			= ((ms,cs),pSt)
		where
			(westt,eastt,score)		= ( west!!tw, east!!te, results!!tw!!te )
		showNextMatch textId progressId matchId compete _ (ls,pSt)
			# pSt					= appPIO (closeTimer matchId) pSt 
			# ((_,scores),pSt)		= checkCompetitionFile compete.west compete.usedRandomSeed pSt
			= (ls,showRanking dialogId (ranking compete.west scores) pSt)

	showRanking dialogId ranking pSt
		# pSt						= rankingToFile pSt
		# pSt						= appPIO (closeAllControls dialogId) pSt
		# (error,pSt)				= openControls dialogId undef resultlist pSt
		| error <> NoError			= abort "Could not refill dialog with result list.\n"
		| otherwise					= pSt
	where
		sorted_ranking				= sortBy (\(_,r1) (_,r2) -> r1 > r2) ranking		// sort ranked list in descending order
		resultlist					=     LayoutControl 
									  (   ListLS [TextControl (toString i) [ControlPos (Left,zero)] \\ i        <- [1..length sorted_ranking]]
									  )   [ControlPos (LeftTop,zero),ControlItemSpace 4 0]
									  :+: LayoutControl
									  (   ListLS [TextControl club         [ControlPos (Left,zero)] \\ (club,_) <- sorted_ranking]
									  )   [ControlPos (RightToPrev,zero),ControlItemSpace 4 0]
									  :+: LayoutControl
									  (   ListLS [TextControl (toString r.matchpoints) [ControlPos (Left,zero)] \\ (_,r) <- sorted_ranking]
									  )   [ControlPos (RightToPrev,zero),ControlItemSpace 4 0]
									  :+: LayoutControl
									  (   ListLS [TextControl (toString r.goals_scored) [ControlPos (Left,zero)] \\ (_,r) <- sorted_ranking]
									  )   [ControlPos (RightToPrev,zero),ControlItemSpace 4 0]
									  :+: LayoutControl
									  (   ListLS [TextControl (toString r.goals_against) [ControlPos (Left,zero)] \\ (_,r) <- sorted_ranking]
									  )   [ControlPos (RightToPrev,zero),ControlItemSpace 4 0]
									  :+: ButtonControl "Ok" [ControlFunction (noLS (closeWindow dialogId)),ControlPos (Right,zero)]
		
		rankingToFile env
			# (ok,file,env)			= fopen "ranking.txt" FWriteText env
			| not ok				= trace_n "Could not output ranking to ranking.txt" env
			# file					= foldl (\file (club,{matchpoints,goals_scored,goals_against}) -> fwrites (club <+++ "\t" <+++ matchpoints <+++ "\t" <+++ goals_scored <+++ "\t" <+++ goals_against <+++ "\n") file) file sorted_ranking
			# (ok,env)				= fclose file env
			| not ok				= trace_n "Could not close ranking.txt" env
			| otherwise				= env

//	Utility functions:
showHalf							:: !Half -> String
showHalf FirstHalf					= "1st half"
showHalf SecondHalf					= "2nd half"

/** getAllTeamsOfHome home field
		yields all teams that start playing at given home and given football field dimensions.
*/
getAllTeamsOfHome					:: !Home !FootballField -> [Team]
getAllTeamsOfHome home field		= apply field (apply home allAvailableTeams)
