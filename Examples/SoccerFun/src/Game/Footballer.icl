implementation module Footballer

import StdEnvExt
import Football, FootballerFunctions, Geometry

instance == Edge					where == North                    North						= True
									      == South                    South						= True
									      == _                        _							= False
instance == FootballerID			where == i1                       i2  						= i1.clubName == i2.clubName && i1.playerNr == i2.playerNr
instance == Half					where == FirstHalf                FirstHalf					= True
									      == SecondHalf               SecondHalf				= True
									      == _                        _							= False
instance == Reprimand				where == Warning                  Warning					= True
									      == YellowCard               YellowCard				= True
									      == RedCard                  RedCard					= True
									      == _                        _							= False
instance == Skill					where == s1                       s2						= toString s1 == toString s2
instance == Success					where == Success                  Success					= True
									      == Fail                     Fail						= True
									      == _                        _							= False
instance == Footballer				where == fb1                      fb2						= fb1.playerID == fb2.playerID
instance == Home					where == West                     West						= True
									      == East                     East						= True
									      == _                        _							= False
instance == FeintDirection			where == FeintLeft                FeintLeft					= True
									      == FeintRight               FeintRight				= True
									      == _                        _							= False
instance == FootballerAction		where == (Move     speed1 angle1) (Move     speed2 angle2)	= speed1 == speed2 && angle1 == angle2
									      == GainBall                 GainBall					= True
									      == (KickBall speed3D1)      (KickBall speed3D2)		= speed3D1 == speed3D2
									      == (HeadBall speed3D1)      (HeadBall speed3D2)		= speed3D1 == speed3D2
									      == (Feint    fd1)           (Feint    fd2)			= fd1 == fd2
									      == (Tackle   tf1 v1)        (Tackle   tf2 v2)			= tf1 == tf2 && v1 == v2
									      == CatchBall                CatchBall					= True
									      == _ _												= False
instance other    Edge				where other North						= South
									      other South						= North
instance other    Half				where other FirstHalf					= SecondHalf
									      other SecondHalf					= FirstHalf
instance other    Home				where other West						= East
									      other East						= West
instance toString Edge				where toString North					= "North"
									      toString South					= "South"
instance toString FootballerID		where toString i  						= "{clubName=" <+++ i.clubName +++ ",playerNr=" <+++ i.playerNr +++ "}"
instance toString Half				where toString FirstHalf				= "FirstHalf"
									      toString SecondHalf				= "SecondHalf"
instance toString Reprimand			where toString Warning					= "Warning"
									      toString YellowCard				= "YellowCard"
									      toString RedCard					= "RedCard"
instance toString Skill				where toString Running					= "Running"
									      toString Dribbling				= "Dribbling"
									      toString Rotating					= "Rotating"
									      toString Gaining					= "Gaining"
									      toString Kicking					= "Kicking"
									      toString Heading					= "Heading"
									      toString Feinting					= "Feinting"
									      toString Jumping					= "Jumping"
									      toString Catching					= "Catching"
									      toString Tackling					= "Tackling"
instance toString Success			where toString Success					= "Success"
									      toString Fail						= "Fail"
instance toString Home				where toString West						= "West"
									      toString East						= "East"
instance toString FeintDirection	where toString FeintLeft				= "FeintLeft"
									      toString FeintRight				= "FeintRight"
instance toString FootballerAction	where toString (Move     speed angle)	= "(Move "     <+++ speed  <+++ " " <+++ angle <+++ ")"
									      toString  GainBall				= "GainBall"
									      toString (KickBall speed)			= "(KickBall " <+++ speed  <+++ ")"
									      toString (HeadBall speed)			= "(HeadBall " <+++ speed  <+++ ")"
									      toString (Feint    fd)			= "(Feint "    <+++ fd     <+++ ")"
									      toString (Tackle   fbID v)		= "(Tackle "   <+++ fbID   <+++ " " <+++ v <+++ ")"
									      toString  CatchBall				= "CatchBall"
instance toString RefereeAction     where toString (ReprimandPlayer fbID reprimand)
																			= "(ReprimandPlayer "  <+++ fbID <+++ " " <+++ reprimand <+++ ")"
										  toString (Hands 			fbID)	= "(Hands "            <+++ fbID <+++ ")"
										  toString (TackleDetected	fbID)	= "(TackleDetected "   <+++ fbID <+++ ")"
										  toString (DangerousPlay	fbID)	= "(DangerousPlay "    <+++ fbID <+++ ")"
										  toString  GameOver				= "GameOver"
										  toString (GameCancelled mHome)	= "(GameCancelled "    <+++ mHome <+++ ")"
										  toString  PauseGame				= "PauseGame"
										  toString (AddTime         time)	= "(AddTime "          <+++ time <+++ ")"
										  toString  EndHalf					= "EndHalf"
										  toString (Goal            home)	= "(Goal "             <+++ home <+++ ")"
										  toString (Offside 		fbID)	= "(Offside "          <+++ fbID <+++ ")"
										  toString (DirectFreeKick  home p) = "(DirectFreeKick "   <+++ home <+++ " " <+++ p <+++ ")"
										  toString (GoalKick        home)	= "(GoalKick "         <+++ home <+++ ")"
										  toString (Corner	      	home e) = "(Corner "           <+++ home <+++ " " <+++ e <+++ ")"
										  toString (ThrowIn         home p) = "(ThrowIn "          <+++ home <+++ " " <+++ p <+++ ")"
										  toString (Penalty		 	home)	= "(Penalty "          <+++ home <+++ ")"
										  toString (CenterKick		home)	= "(CenterKick "       <+++ home <+++ ")"
										  toString (Advantage		home)	= "(Advantage "        <+++ home <+++ ")"
										  toString (OwnBallIllegally fbID)	= "(OwnBallIllegally " <+++ fbID <+++ ")"
										  toString (DisplacePlayers	displs)	= "(DisplacePlayers [" <+++ showList "," displs <+++ "])"
										  toString  ContinueGame			= "ContinueGame"
										  toString (TellMessage		msg)	= "(TellMessage \""    <+++ msg <+++ "\")"

::	Minutes =   Minutes !Real

instance zero         Minutes		where zero								= Minutes zero
instance <            Minutes		where <  (Minutes m1) (Minutes m2)		= m1 < m2
instance ==           Minutes		where == (Minutes m1) (Minutes m2)		= m1 == m2
instance +            Minutes		where +  (Minutes m1) (Minutes m2)		= Minutes (m1+m2)
instance -            Minutes		where -  (Minutes m1) (Minutes m2)		= Minutes (m1-m2)
instance scale        Minutes       where scale         k (Minutes m)       = Minutes (k * m)
instance toString     Minutes		where toString        (Minutes m)		= toString (s/60) <+++ ":" <+++ if (s mod 60 < 10) "0" "" <+++ (s mod 60) <+++" min"
                                          where s							= toInt (((toReal (toInt (m * 100.0)))/100.0) * 60.0)
instance toReal       Minutes		where toReal (Minutes m)				= m
instance minutes      Real			where minutes m							= Minutes m

instance toPosition   Footballer	where toPosition   fb					= fb.pos
instance toPosition3D Footballer	where toPosition3D fb					= toPosition3D fb.pos
instance nameOf       Footballer	where nameOf {name,nose}				= name
instance nameOf       FootballerID	where nameOf {clubName}					= clubName
instance sameClub     FootballerID	where sameClub id1 id2					= nameOf id1 == nameOf id2
instance sameClub     Footballer	where sameClub fb1 fb2					= sameClub fb1.playerID fb2.playerID

defaultFootballer							:: !FootballerID -> Footballer
defaultFootballer playerID					= { playerID	= playerID
											  , name		= "default"
											  , length		= m 1.6
											  , pos			= zero 
											  , speed		= zero
											  , nose		= zero
											  , skills		= (Running, Kicking, Dribbling)
											  , effect		= Nothing
											  , stamina		= max_stamina
											  , health		= max_health
											  , brain		= {memory=Void, ai=returnAI (Move zero zero)}
											  }

inRadiusOfFootballer						:: !Position !Footballer -> Bool
inRadiusOfFootballer pos player				= isbetween pos.px (player.pos.px - xWidthFootballer) (player.pos.px + xWidthFootballer) &&
											  isbetween pos.py (player.pos.py - yWidthFootballer) (player.pos.py + yWidthFootballer)

skillsAsList								:: !Footballer -> [Skill]
skillsAsList fb								= (\(a,b,c)->[a,b,c]) fb.skills

identify_player								:: !FootballerID !Footballer -> Bool
identify_player id fb						= id == fb.playerID

player_identity								:: !Footballer -> FootballerID
player_identity fb							= fb.playerID

getClubName									:: !Footballer -> ClubName
getClubName fb								= nameOf fb.playerID

isKeeper									:: !Footballer -> Bool
isKeeper fb									= fb.playerID.playerNr == 1

isFielder									:: !Footballer -> Bool
isFielder fb								= not (isKeeper fb)

/**	Footballer attribute dependent abilities:
*/
maxGainReach								:: !Footballer -> Metre
maxGainReach fb								= scale (if (isMember Gaining (skillsAsList fb)) 0.5 0.3) fb.length

maxJumpReach								:: !Footballer -> Metre
maxJumpReach fb								= scale (if (isMember Jumping (skillsAsList fb)) 0.6 0.4) fb.length

maxGainVelocityDifference					:: !Footballer !Metre -> Velocity
maxGainVelocityDifference fb d_player_ball	= ms (if (isMember Gaining (skillsAsList fb)) 15.0 10.0 -  distanceDifficulty)
where
	length									= toReal fb.length
	distanceDifficulty						= max zero ((0.8 * length)^4.0 * ((toReal d_player_ball)/length))

maxCatchVelocityDifference					:: !Footballer !Metre -> Velocity
maxCatchVelocityDifference fb d_player_ball	= ms (if (isMember Gaining (skillsAsList fb)) 20.0 17.0 - distanceDifficulty)
where
	length									= toReal fb.length
	distanceDifficulty						= max zero ((0.8 * length)^4.0 * ((toReal d_player_ball)/length))

maxKickReach								:: !Footballer -> Metre
maxKickReach fb								= scale (if (isMember Kicking (skillsAsList fb)) 0.6 0.4) fb.length

maxHeadReach								:: !Footballer -> Metre
maxHeadReach fb								= scale (if (isMember Heading (skillsAsList fb)) 0.4 0.2) fb.length

maxCatchReach								:: !Footballer -> Metre		// includes horizontal jumping
maxCatchReach fb							= scale (if (isMember Catching (skillsAsList fb)) 1.8 1.5) fb.length

maxTackleReach								:: !Footballer -> Metre
maxTackleReach fb							= scale (if (isMember Tackling (skillsAsList fb)) 0.33 0.25) fb.length

maxVelocityBallKick							:: !Footballer -> Velocity	
maxVelocityBallKick fb						= ms ((if (isMember Kicking (skillsAsList fb)) 27.0 25.0 + (toReal fb.length)/2.0) * (0.2*fatHealth+0.8))
where
	fatHealth								= getHealthStaminaFactor fb.health fb.stamina

maxVelocityBallHead							:: !Footballer !Velocity -> Velocity
maxVelocityBallHead fb ballSpeed			= scale 0.7 ballSpeed + scale (0.1*fatHealth+0.9) (ms (if (isMember Heading (skillsAsList fb)) 7.0 5.0))
where
	fatHealth								= getHealthStaminaFactor fb.health fb.stamina
	
maxKickingDeviation							:: !Footballer -> Angle
maxKickingDeviation skills					= rad (0.5*pi) //if (isMember Kicking skills) (pi/18.0) (pi/2.0)

maxHeadingDeviation							:: !Footballer -> Angle
maxHeadingDeviation skills					= rad (0.25*pi) //if (isMember Heading skills) (pi/16.0) (pi/5.0)

maxRotateAngle								:: !Footballer -> Angle
maxRotateAngle fb=:{speed,length}
| velocity < 1.0							= rad pi
| otherwise									= rad (pi/18.0*((5.0/velocity)*((toReal length)/2.0)))
where
	velocity								= abs (toReal speed.velocity)

maxFeintStep								:: !Footballer -> Metre
maxFeintStep fb								= m (if (isMember Feinting (skillsAsList fb)) 0.75 0.5)

::	HealthStaminaFactor						:== Real	 	// combination of stamina and health

getHealthStaminaFactor						:: !Health !Stamina -> HealthStaminaFactor
getHealthStaminaFactor health stamina
| stamina <= health							= stamina
| otherwise									= avg [stamina,health]

isMove										:: !FootballerAction -> Bool
isMove (Move _ _)							= True
isMove _									= False

isGainBall									:: !FootballerAction -> Bool
isGainBall GainBall							= True
isGainBall _								= False

isKickBall									:: !FootballerAction -> Bool
isKickBall (KickBall _)						= True
isKickBall _								= False

isHeadBall									:: !FootballerAction -> Bool
isHeadBall (HeadBall _)						= True
isHeadBall _								= False

isFeint										:: !FootballerAction -> Bool
isFeint (Feint _)							= True
isFeint _									= False

isFootballerTackle							:: !FootballerAction -> Bool
isFootballerTackle (Tackle _ _)				= True
isFootballerTackle _						= False

isCatchBall									:: !FootballerAction -> Bool
isCatchBall CatchBall						= True
isCatchBall _								= False

isActionOnBall								:: !FootballerAction -> Bool
isActionOnBall GainBall						= True
isActionOnBall CatchBall					= True
isActionOnBall (KickBall _)					= True
isActionOnBall (HeadBall _)					= True
isActionOnBall _							= False

getDefaultField								:: FootballField
getDefaultField								= { fwidth = m 75.0, flength = m 110.0 }

inPenaltyArea								:: !FootballField !Home !Position -> Bool
inPenaltyArea field home pos				= isbetween pos.py south_edge north_edge && if (home == West) (pos.px <= west_edge) (pos.px >= east_edge)
where
	north_edge								= northPole + radius_penalty_area
	south_edge								= southPole - radius_penalty_area
	(northPole,southPole)					= goal_poles field
	half_length								= scale 0.5 field.flength
	west_edge								= penalty_area_depth - half_length
	east_edge								= half_length - penalty_area_depth

goal_poles									:: !FootballField -> (!Metre,!Metre)
goal_poles field							= (half_goal_width,~half_goal_width)
where
	half_goal_width							= scale 0.5 goal_width

isMoved										:: !FootballerEffect -> Bool
isMoved (Moved _ _)							= True
isMoved _									= False

isGainedBall								:: !FootballerEffect -> Bool
isGainedBall (GainedBall _)					= True
isGainedBall _								= False

isKickedBall								:: !FootballerEffect -> Bool
isKickedBall (KickedBall _)					= True
isKickedBall _								= False

isHeadedBall								:: !FootballerEffect -> Bool
isHeadedBall (HeadedBall _)					= True
isHeadedBall _								= False

isFeinted									:: !FootballerEffect -> Bool
isFeinted (Feinted _)						= True
isFeinted _									= False

isTackled									:: !FootballerEffect -> Bool
isTackled (Tackled _ _ _)					= True
isTackled _									= False

isCaughtBall								:: !FootballerEffect -> Bool
isCaughtBall (CaughtBall _)					= True
isCaughtBall _								= False

isOnTheGround								:: !FootballerEffect -> Bool
isOnTheGround (OnTheGround _)				= True
isOnTheGround _								= False

failFootballerAction						:: !FootballerAction -> FootballerEffect
failFootballerAction (Move   s a)			= Moved    s a
failFootballerAction GainBall				= GainedBall Fail
failFootballerAction CatchBall				= CaughtBall Fail
failFootballerAction (KickBall v)			= KickedBall Nothing
failFootballerAction (HeadBall v)			= HeadedBall Nothing
failFootballerAction (Feint    d)			= Feinted d
failFootballerAction (Tackle p v)			= Tackled p v Fail
failFootballerAction _						= abort "failFootballerAction: unknown action failed"

displacements								:: !Team -> Displacements
displacements team							= [(playerID,pos) \\ {playerID,pos} <- team]

showSuccintRefereeAction					:: !RefereeAction -> String
showSuccintRefereeAction refAction
	= case refAction of
		(ReprimandPlayer  id r)				= player id                     <+++ " receives " <+++ r
		(Hands            id)				= "Hands by "                   <+++ player id
		(TackleDetected   id)				= "Tackle by "                  <+++ player id
		(DangerousPlay    id)				= "Dangerous play by "          <+++ player id
		GameOver							= "Game ended"
		(GameCancelled    mt)				= "Game cancelled"              <+++ if (isJust mt) (" winner is " <+++ fromJust mt) ""
		PauseGame							= "Game paused"
		(AddTime  t)						= "Extra time added: "          <+++ t
		EndHalf								= "First half ended"
		(Goal     t)						= "Goal for "                   <+++ t
		(Offside        id)					= "Offside by "                 <+++ player id
		(DirectFreeKick t p)				= "Direct free kick for "       <+++ t
		(GoalKick t)						= "Goal kick for "              <+++ t
		(Corner   t e)						= "Corner for "                 <+++ t
		(ThrowIn  t p)						= "Throw in for "               <+++ t
		(Penalty    t)						= "Penalty for "                <+++ t
		(CenterKick t)						= "Center kick for "            <+++ t
		(Advantage t)						= "Advantage for "              <+++ t
		(OwnBallIllegally id)				= "Illegal ball possession by " <+++ player id
		(DisplacePlayers _)					= "Players displaced"
		ContinueGame						= "Game continued"
		(TellMessage txt)					= txt
where
	player {clubName,playerNr}				= clubName <+++"[" <+++ playerNr <+++ "]"

isReprimandPlayer							:: !RefereeAction -> Bool
isReprimandPlayer (ReprimandPlayer _ _)		= True
isReprimandPlayer _							= False

isHands										:: !RefereeAction -> Bool
isHands (Hands _)							= True
isHands _									= False

isTackleDetected							:: !RefereeAction -> Bool
isTackleDetected (TackleDetected _)			= True
isTackleDetected _							= False

isDangerousPlay								:: !RefereeAction -> Bool
isDangerousPlay (DangerousPlay _)			= True
isDangerousPlay _							= False

isGameOver									:: !RefereeAction -> Bool
isGameOver GameOver							= True
isGameOver _								= False

isGameCancelled								:: !RefereeAction -> Bool
isGameCancelled (GameCancelled _)			= True
isGameCancelled _							= False

isPauseGame									:: !RefereeAction -> Bool
isPauseGame PauseGame						= True
isPauseGame _								= False

isAddTime									:: !RefereeAction -> Bool
isAddTime (AddTime _)						= True
isAddTime _									= False

isEndHalf									:: !RefereeAction -> Bool
isEndHalf EndHalf							= True
isEndHalf _									= False

isGoal										:: !RefereeAction -> Bool
isGoal (Goal _)								= True
isGoal _									= False

isOffside									:: !RefereeAction -> Bool
isOffside (Offside _)						= True
isOffside _									= False

isDirectFreeKick							:: !RefereeAction -> Bool
isDirectFreeKick (DirectFreeKick _ _ )		= True
isDirectFreeKick _							= False

isGoalKick									:: !RefereeAction -> Bool
isGoalKick (GoalKick _)						= True
isGoalKick _								= False

isCorner									:: !RefereeAction -> Bool
isCorner (Corner _ _)						= True
isCorner _									= False

isThrowIn									:: !RefereeAction -> Bool
isThrowIn (ThrowIn _ _)						= True
isThrowIn _									= False

isPenalty									:: !RefereeAction -> Bool
isPenalty (Penalty _)						= True
isPenalty _									= False

isCenterKick								:: !RefereeAction -> Bool
isCenterKick (CenterKick _)					= True
isCenterKick _								= False

isAdvantage									:: !RefereeAction -> Bool
isAdvantage (Advantage _)					= True
isAdvantage _								= False

isOwnBallIllegally							:: !RefereeAction -> Bool
isOwnBallIllegally (OwnBallIllegally _)		= True
isOwnBallIllegally _						= False

isDisplacePlayers							:: !RefereeAction -> Bool
isDisplacePlayers (DisplacePlayers _)		= True
isDisplacePlayers _							= False

isContinueGame								:: !RefereeAction -> Bool
isContinueGame ContinueGame					= True
isContinueGame _							= False

isTellMessage								:: !RefereeAction -> Bool
isTellMessage (TellMessage _)				= True
isTellMessage _								= False

getKickPos									:: !FootballField !Half !RefereeAction -> Maybe Position
getKickPos field half (GoalKick home)		= Just { zero & px = if (home == West) (penalty_area_depth - half_length) (half_length - penalty_area_depth) }
where
	half_length								= scale 0.5 field.flength
getKickPos field half (Corner home edge)	= Just { px = if (home == East)
											                 (half_radius_corner_kick_area - half_length)
											                 (half_length - half_radius_corner_kick_area)
											       , py = if (edge == North)
											                 (half_radius_corner_kick_area - half_width)
											                 (half_width - half_radius_corner_kick_area)
											       }		
where
	half_width								= scale 0.5 field.fwidth
	half_length								= scale 0.5 field.flength
	half_radius_corner_kick_area			= scale 0.5 radius_corner_kick_area
getKickPos field half (Penalty home)		= Just { zero & px = if (home == East)
											                        (penalty_spot_depth - half_length)
											                        (half_length - penalty_spot_depth)
											       }
where
	half_length								= scale 0.5 field.flength
getKickPos field _ (CenterKick _)			= Just zero
getKickPos _ _ (DirectFreeKick _ pos)		= Just pos
getKickPos _ _ (ThrowIn _ pos)				= Just pos
getKickPos _ _ _							= Nothing
