definition module Footballer

/**	This module defines the part of the SoccerFun API that is concerned with the footballer data types.
*/
import StdMaybe, StdEnvExt
from   StdIOCommon import :: Void(..)
import Football, Geometry, Team

::	Footballer				= E. memory:											// The memory of a footballer is a black box
								{ playerID	:: !FootballerID						// The identification of a player: this must be unique
								, name		:: !String								// The name of a player: this need not be unique
								, length	:: !Length								// The length of a player: should be in range [min_length..max_length]
								, pos		:: !Position							// The position of a player: should be on the football field
								, speed		:: !Speed								// The speed of a player: absolute direction and velocity with which player is moving
								, nose		:: !Angle								// The bearing of a player: absolute direction in which player is looking
								, skills	:: !MajorSkills							// The major skills of a player: these improve performance of affected actions
								, effect	:: !Maybe FootballerEffect				// The effect(s) of the previous action
								, stamina	:: !Stamina								// The current stamina of a player: 1.0 is optimal, 0.0 is worst
								, health	:: !Health								// The current health  of a player: 1.0 is optimal, 0.0 is worst
								, brain		:: !Brain (FootballerAI memory) memory	// The precious asset: use and update the memory and compute an action
								}
::	FootballerID			=	{ clubName	:: !ClubName							// Club name of team
								, playerNr	:: !PlayersNumber						// Number of player (1 for keeper, other for fielders)
								}
instance ==       FootballerID
instance toString FootballerID
instance nameOf   FootballerID

class sameClub a			:: !a !a -> Bool										// belong to same club
instance sameClub FootballerID
instance sameClub Footballer

::	ClubName				:== String
::	Length					:== Metre
::	MajorSkills				:== (!Skill,!Skill,!Skill)
::	Skill					=	Running												// Faster running without ball in possession
							|	Dribbling											// Faster running with ball in possession
							|	Rotating											// Wider range of rotation
							|	Gaining												// Better ball gaining ability
							|	Kicking												// More accurate and wider ball kicking
							|	Heading												// More accurate and wider ball heading
							|	Feinting											// Wider range of feint manouvre
							|	Jumping												// Further jumping
							|	Catching											// Better catching
							| 	Tackling											// More effective tackling
instance ==       Skill
instance toString Skill
::	PlayersNumber			:== Int

instance ==           Footballer													// two players are equal if their playerID's are equal
instance toPosition   Footballer
instance toPosition3D Footballer

defaultFootballer			:: !FootballerID -> Footballer

inRadiusOfFootballer		:: !Position !Footballer -> Bool						// True iff position touches/hits footballer
skillsAsList				::           !Footballer -> [Skill]						// Skills of the footballer as a list

::	FootballerAI memory		:== (BrainInput,memory) -> (BrainOutput,memory)
::	FootballerAI`			:==  BrainInput         ->  BrainOutput
::	BrainInput				=	{ referee	:: [RefereeAction]						// the referee actions
								, football	:: FootballState						// the state of the football
								, others	:: [Footballer]							// all other football players
								, me		:: Footballer							// the player himself
								}
::	BrainOutput				:== FootballerAction									// the footballer action

::	Half					= FirstHalf | SecondHalf
instance ==					Half
instance toString			Half
instance other				Half

::	Home					= West | East
instance ==					Home
instance toString			Home
instance other				Home

xWidthFootballer			:== m (0.7/2.0)											// chest   size of footballer
yWidthFootballer			:== m (0.4/2.0)											// stomach size of footballer

identify_player 			:: !FootballerID !Footballer -> Bool
player_identity				:: !Footballer -> FootballerID

class    nameOf a   		:: !a -> String
instance nameOf Footballer

getClubName					:: !Footballer -> ClubName
isKeeper					:: !Footballer -> Bool
isFielder					:: !Footballer -> Bool


::	Brain ai m				=	{ ai	:: !ai
								, memory:: !m
								}
::	Stamina					:== Real
::	Health					:== Real

min_length					:== m 1.6												// minimum length of a person. Advantages:  better gainball; better stamina at sprinting; better dribbling; less health damage when fall, better rotating.
max_length					:== m 2.1												// maximum length of a person. Advantages:	wider  gainball; better stamina at running;   higher headball;  improved catching; harder kicking.
max_stamina					:== 1.0
max_health					:== 1.0

/**	Footballer attribute dependent abilities:
		use these functions to make your player correctly dependent of abilities.
*/
maxGainReach				:: !Footballer				-> Metre
maxJumpReach				:: !Footballer				-> Metre					// vertical jumping
maxGainVelocityDifference	:: !Footballer !Metre		-> Velocity
maxCatchVelocityDifference	:: !Footballer !Metre		-> Velocity
maxKickReach				:: !Footballer				-> Metre
maxHeadReach				:: !Footballer				-> Metre
maxCatchReach				:: !Footballer				-> Metre					// includes horizontal jumping
maxTackleReach				:: !Footballer				-> Metre
maxVelocityBallKick			:: !Footballer				-> Velocity	
maxVelocityBallHead			:: !Footballer !Velocity	-> Velocity
maxKickingDeviation			:: !Footballer				-> Angle
maxHeadingDeviation			:: !Footballer				-> Angle
maxRotateAngle				:: !Footballer				-> Angle					// maximum angle with which footballer can rotate
maxFeintStep				:: !Footballer				-> Metre					// maximum side step of footballer for feint manouvre

::	HealthStaminaFactor		:== Real	 											// combination of stamina and health
getHealthStaminaFactor		:: !Health !Stamina			-> HealthStaminaFactor


::	FootballField			= { fwidth	:: !FieldWidth								// width  of football field (64m <=width <=75m)
							  , flength	:: !FieldLength								// length of football field (100m<=length<=110m)
							  }
::	FieldWidth				:== Metre
::	FieldLength				:== Metre

getDefaultField				:: FootballField
inPenaltyArea				:: !FootballField !Home !Position -> Bool

/**	Official metrics of a football field:
*/
radius_centre_circle	 	:== m  9.15
radius_centre_spot			:== m  0.3												// not official, taken for rendering
goal_width					:== m  7.32												// interior
goal_height					:== m  2.44												// interior
goal_area_depth				:== m  5.50
penalty_area_depth			:== m 16.50
penalty_spot_depth			:== m 11.00
radius_penalty_spot			:== m  0.3												// not official, taken for rendering
radius_penalty_area			:== m  9.15
radius_corner_kick_area		:== m  0.90
goal_pole_width				:== m  0.2												// not official

/** goal_poles yields the py coordinates of the interiors of (north pole, south pole) of the goal (note that north > 0 > south)
*/	
goal_poles					:: !FootballField	-> (!Metre,!Metre)

repell_distance				:== m 9.15												// minimum distance of opponents from ball at (in)direct free kick

::	FootballerAction																// actions a player can intend to perform
	=	Move				!Speed !Angle											// wish to rotate over given angle, and then move with given speed
	|	Feint				!FeintDirection											// wish to make feint manouvre
	|	KickBall			!Speed3D												// wish to kick ball with given speed
	|	HeadBall			!Speed3D												// wish to head ball with given speed
	|	GainBall																	// wish to gain possession of the ball
	| 	CatchBall																	// wish to catch the ball with his hands
	| 	Tackle				!FootballerID !Velocity									// wish to tackle identified player, higher velocity is higher chance of succes AND injury

isMove						:: !FootballerAction -> Bool
isGainBall					:: !FootballerAction -> Bool
isCatchBall					:: !FootballerAction -> Bool
isKickBall					:: !FootballerAction -> Bool
isHeadBall					:: !FootballerAction -> Bool
isFeint						:: !FootballerAction -> Bool
isFootballerTackle			:: !FootballerAction -> Bool
isActionOnBall				:: !FootballerAction -> Bool

instance ==					FootballerAction
instance toString			FootballerAction

:: FeintDirection			= FeintLeft | FeintRight
instance ==					FeintDirection
instance toString			FeintDirection

::	FootballerEffect		= Moved       !Speed !Angle								// player has rotated with given angle, and then ran with given speed
							| Feinted     !FeintDirection							// player had feinted
							| KickedBall  !(Maybe Speed3D)							// player kicked ball (Just v) with velocity, or didn't (Nothing)
							| HeadedBall  !(Maybe Speed3D)							// player headed ball (Just v) with velocity, or didn't (Nothing)
							| GainedBall  !Success									// player attempt to gain ball from other player
							| CaughtBall  !Success									// player caught the ball with his hands
							| Tackled     !FootballerID !Velocity !Success			// player attempt to tackle an opponent
							| OnTheGround !FramesToGo								// tackled by someone else; FramesToGo is the amount of frames that you will be on the ground
::	Reprimand				= Warning | YellowCard | RedCard						// If the referee gives a second yellow he should add red to it himself
instance toString			Reprimand
instance ==					Reprimand
::	Success					= Success | Fail
instance toString			Success
instance ==					Success
::	FramesToGo				:== Int													// number of frames to go before event ends				

isMoved						:: !FootballerEffect -> Bool
isGainedBall				:: !FootballerEffect -> Bool
isKickedBall				:: !FootballerEffect -> Bool
isHeadedBall				:: !FootballerEffect -> Bool
isFeinted					:: !FootballerEffect -> Bool
isTackled					:: !FootballerEffect -> Bool
isCaughtBall				:: !FootballerEffect -> Bool
isOnTheGround				:: !FootballerEffect -> Bool

failFootballerAction		:: !FootballerAction -> FootballerEffect

::	RefereeAction			= ReprimandPlayer   !FootballerID !Reprimand			// player with given name receives reprimand
							| Hands 			!FootballerID						// person is seen for doing hands
							| TackleDetected	!FootballerID						// person is seen for doing tackle
							| DangerousPlay		!FootballerID						// person is seen for doing dangerous actions
							| GameOver	 											// end of game
							| GameCancelled		!(Maybe Home)						// game is cancelled, (Just home) wins
							| PauseGame												// game is paused
							| AddTime 			!ExtraTime							// extra time is added to the game
							| EndHalf												// first half is over, teams go for a second half
							| Goal 				!Home 								// team playing at home has scored
							| Offside 			!FootballerID						// player is offside
							| DirectFreeKick   	!Home !Position						// a direct free kick is granted for team home at given position					
							| GoalKick         	!Home								// a goal kick is granted for team home
							| Corner	      	!Home !Edge							// a corner kick is granted for team home
							| ThrowIn          	!Home !Position						// a throw in ball is granted for team home at given position
							| Penalty		 	!Home								// penalty at homeside
							| CenterKick		!Home								// team playing at home may start from the center
							| Advantage			!Home								// referee gives advantages to home-team
							| OwnBallIllegally  !FootballerID						// ball was for the other team
							| DisplacePlayers	!Displacements						// displaces all footballers at the provided position (used with free kicks)
							| ContinueGame
							| TellMessage		!String								// no effect on match, message is displayed by referee
instance toString RefereeAction														// yield a complete, yet verbose, string representation of a referee decision
showSuccintRefereeAction	:: !RefereeAction -> String								// yield a concise string representation of the referee decision

::	Edge					= North | South
instance ==					Edge
instance toString			Edge
instance other				Edge

::	Displacements			:== AssocList FootballerID Displacement					// players that need to be displaced
::	Displacement			:== Position											// new position

displacements				:: !Team -> Displacements

::	ExtraTime				:== Minutes
::	Minutes
instance zero				Minutes
instance <					Minutes
instance ==					Minutes
instance +					Minutes
instance -					Minutes
instance scale				Minutes
instance toString			Minutes
instance toReal				Minutes

class minutes a				:: !a -> Minutes
instance minutes			Real													// (minutes m) interprets m as number of minutes

isReprimandPlayer			:: !RefereeAction -> Bool
isHands						:: !RefereeAction -> Bool
isTackleDetected			:: !RefereeAction -> Bool
isDangerousPlay				:: !RefereeAction -> Bool
isGameOver					:: !RefereeAction -> Bool
isGameCancelled				:: !RefereeAction -> Bool
isPauseGame					:: !RefereeAction -> Bool
isAddTime					:: !RefereeAction -> Bool
isEndHalf					:: !RefereeAction -> Bool
isGoal						:: !RefereeAction -> Bool
isOffside					:: !RefereeAction -> Bool
isDirectFreeKick			:: !RefereeAction -> Bool
isGoalKick					:: !RefereeAction -> Bool
isCorner					:: !RefereeAction -> Bool
isThrowIn					:: !RefereeAction -> Bool
isPenalty					:: !RefereeAction -> Bool
isCenterKick				:: !RefereeAction -> Bool
isAdvantage					:: !RefereeAction -> Bool
isOwnBallIllegally			:: !RefereeAction -> Bool
isDisplacePlayers			:: !RefereeAction -> Bool
isContinueGame				:: !RefereeAction -> Bool
isTellMessage				:: !RefereeAction -> Bool

getKickPos					:: !FootballField !Half !RefereeAction -> Maybe Position
