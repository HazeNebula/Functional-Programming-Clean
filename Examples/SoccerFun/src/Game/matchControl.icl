implementation module matchControl

import StdEnvExt
import Gui2D				// we choose the 2D GUI version of SoccerFun
import Referee

::	Seconds = Seconds !Real

s :: !Real -> Seconds						// (s x) represents x seconds of time
s x											= Seconds x

instance zero      Seconds where zero								= Seconds zero
instance ==        Seconds where ==        (Seconds s1) (Seconds s2)= s1 == s2
instance <         Seconds where <         (Seconds s1) (Seconds s2)= s1  < s2
instance +         Seconds where +         (Seconds s1) (Seconds s2)= Seconds (s1  + s2)
instance -         Seconds where -         (Seconds s1) (Seconds s2)= Seconds (s1  - s2)
instance minutes   Seconds where minutes   (Seconds s)				= minutes (s/60.0)
instance toReal    Seconds where toReal    (Seconds s)              = s
instance scale     Seconds where scale     k            (Seconds s) = Seconds (k * s)
instance toString  Seconds where toString  (Seconds s)				= s +++> " sec."

doSoccerFun :: !*World -> *World
doSoccerFun world							= SoccerFunGUI2D world

simulationGranularity :: Seconds
simulationGranularity = s 0.05

// the original simulation expected to run at 0.1s; this function is used to adjust it to different rates
rateAdjust :: (Real -> Real)
rateAdjust = case simulationGranularity of
					Seconds 0.1  -> id
					Seconds 0.05 -> sqrt
					Seconds x	 -> \y -> y^(10.0*x)

setMatchStart :: !Team !Team !FootballField !Referee !PlayingTime !RandomSeed -> Match
setMatchStart fstTeam sndTeam field referee time rs
	= { team1								= validateTeam fstTeam
	  , team2								= validateTeam sndTeam
	  , theBall								= Free zero
	  , theField							= field
	  , theReferee							= referee
	  , playingHalf							= FirstHalf
	  , playingTime							= time
	  , unittime							= simulationGranularity
	  , score								= (0,0)
	  , nextRandomP							= nextRandomP
	  , seed								= rs
	  , lastContact							= Nothing
	  }

stepMatch :: !Match -> (!(![RefereeAction],!AssocList FootballerID FootballerAction),!Match)
stepMatch match
# (refereeActions,  match)					= refereeTurn                                     match
# match										= performRefereeActions refereeActions            match
# (intendedActions, match)					= playersThink          refereeActions            match
# (okActions,       match)					= successfulActions     intendedActions           match
# match										= doFootballerActions   intendedActions okActions match
# match										= moveFootball                                    match
# match										= advanceTime                                     match
= ((refereeActions,okActions),match)
where
/*	refereeTurn match
		determines whether the rules of soccer are adhered to and yields a list of referee actions.
*/	refereeTurn :: !Match -> (![RefereeAction],!Match)
	refereeTurn match=:{theReferee=referee=:{Referee | brain=brain=:{ai,memory}},theBall,playingHalf,team1,team2,playingTime,unittime,seed,lastContact}
											= (refereeActions,{match & theReferee=new_referee,seed=new_seed})
	where
		(refereeActions,(memory`,new_seed))	= ai ({RefereeInput | playingTime = playingTime
		                                                        , unittime    = unittime
		                                                        , theBall     = theBall
		                                                        , playingHalf = playingHalf
		                                                        , team1       = team1
		                                                        , team2       = team2
		                                                        , lastContact = lastContact
		                                          }
		                                         ,(memory,seed)
		                                         )
		new_referee							= {Referee | referee & brain={Brain | brain & memory=memory`}}
		
/*	performRefereeActions refereeActions match
		performs for each football player in match his succeededAction, informs them about the referee actions, and moves the ball. 
*/	performRefereeActions :: ![RefereeAction] !Match -> Match
	performRefereeActions refActions match	= foldl doRefereeEvent match refActions
	where
		doRefereeEvent :: !Match !RefereeAction -> Match
		doRefereeEvent theMatch=:{Match | playingHalf,theField,team1,team2} refereeAction
		| isAlterMatchBallAndTeams			= {Match | theMatch & theBall=Free (mkFootball pos zero),lastContact=Nothing}
		| isProgressEvent					= gameProgress theMatch
		| isDisplaceTeamsEvent				= {Match | theMatch & team1=map (displacePlayer ds) team1,team2=map (displacePlayer ds) team2}
		| isReprimandEvent					= let (team1`,team2`)		= reprimandPlayer rep (team1,team2) in {Match | theMatch & team1=team1`,team2=team2`}
		| otherwise							= theMatch
		where
			(isAlterMatchBallAndTeams,pos)	= case refereeAction of
												DirectFreeKick _ pos	= (True,pos)
												ThrowIn        _ pos	= (True,pos)
												Corner         _ _		= (True,fromJust (getKickPos theField playingHalf refereeAction))
												GoalKick       _		= (True,fromJust (getKickPos theField playingHalf refereeAction))
												Penalty        _		= (True,fromJust (getKickPos theField playingHalf refereeAction))
												CenterKick     _		= (True,fromJust (getKickPos theField playingHalf refereeAction))
												otherwise				= (False,undef)
			(isProgressEvent,gameProgress)	= case refereeAction of
												GameOver				= (True,\m                -> {Match | m & playingTime=zero})
												GameCancelled mt		= (True,\m                -> {Match | m & playingTime=zero,score=case mt of
																		                                                                    Nothing   = (0,0)
																		                                                                    Just West = if (playingHalf==FirstHalf) (1,0) (0,1)
																		                                                                    just_east = if (playingHalf==FirstHalf) (0,1) (1,0)
																		                             })
												AddTime t				= (True,\m                -> {Match | m & playingTime=m.Match.playingTime+t})
												EndHalf					= (True,\m                -> {Match | m & playingHalf=SecondHalf})
												Goal    h				= (True,\m=:{score=(w,e)} -> {Match | m & score=if (h==West && playingHalf==FirstHalf || h==East && playingHalf==SecondHalf) (w+1,e) (w,e+1)})
												otherwise				= (False,undef)
			(isDisplaceTeamsEvent,ds)		= case refereeAction of
												DisplacePlayers ds		= (True, ds)
												otherwise				= (False,undef)
			(isReprimandEvent,rep)			= case refereeAction of
												ReprimandPlayer p r		= (True, (p,r))
												otherwise				= (False,undef)
			
			displacePlayer :: !Displacements !Footballer -> Footballer
			displacePlayer displacements fb	= case lookup fb.playerID displacements of
												Just pos	= {fb & pos=pos}
												nothing		= fb
			
			reprimandPlayer :: !(!FootballerID,!Reprimand) !(![Footballer],![Footballer]) -> (![Footballer],![Footballer])
			reprimandPlayer (playerID,RedCard) (team1,team2)
											= splitAt (nr_players_1 - if (playerID.clubName == club1) 1 0) (uneq1++uneq2)
			where
				club1						= nameOf team1
				(uneq1,_,uneq2)				= break1 (identify_player playerID) (team1++team2)
				nr_players_1				= length team1
			reprimandPlayer _ teams			= teams			
	
/*	playersThink match
		lets every footballer player conjure an initiative.
*/	playersThink :: ![RefereeAction] !Match -> (!AssocList FootballerID FootballerAction,!Match)
	playersThink refereeActions match=:{Match | theBall,team1,team2}
											= (intendedActions,new_match)
	where
		actionsOfTeam1						= map (think refereeActions theBall team2) (singleOutElems team1)
		actionsOfTeam2						= map (think refereeActions theBall team1) (singleOutElems team2)
		new_match							= {Match | match & team1 = map snd actionsOfTeam1,team2 = map snd actionsOfTeam2}
		intendedActions						= [(playerID,action) \\ (action,{playerID}) <- actionsOfTeam1 ++ actionsOfTeam2]
		
		think :: ![RefereeAction] !FootballState ![Footballer] !(!Footballer,![Footballer]) -> (!FootballerAction,!Footballer)
		think refereeActions ballstate opponents (me=:{Footballer | brain=brain=:{ai,memory}},ownTeam)
		# (action,memory)					= ai ({referee=refereeActions,football=ballstate,others=ownTeam ++ opponents,me=me},memory)
		# me								= {Footballer | me & brain = {Brain | brain & memory=memory}}
		= (action,me)
		
/*	successfulActions intendedActions match
		removes all failing intended actions, and returns the list of remaining succeeding actions.
		Players who are successfully tackled fail their action.
		Players who are (still) lying on the ground fail their action.
		At most one action of {GainBall, KickBall, HeadBall, CatchBall} succeeds.
		If another player has successfully played the ball then his/her playerID is registered in Match.
*/	successfulActions :: !(AssocList FootballerID FootballerAction) !Match -> (!AssocList FootballerID FootballerAction,!Match)
	successfulActions intendedActions match=:{seed,lastContact,nextRandomP,team1,team2,theBall}
	# otherActions							= filter (\(playerID,_) -> not (isMember playerID groundVictims)) intendedActions
	# (tackleActions,otherActions)			= spanfilter (isFootballerTackle o snd) intendedActions
	# (okTackleActions,seed)				= selectTackleActions tackleActions seed
	# tackleVictims							= [victim \\ (_,Tackle victim _) <- okTackleActions]
	# otherActions							= filter (\(playerID,action) -> not (isMember playerID tackleVictims)) otherActions
	# (ballActions,otherActions)			= spanfilter (isActionOnBall o snd) otherActions
	# (okBallAction,seed)					= selectBallAction ballActions seed
	# (okActions,newContact)				= case okBallAction of
											     Just (player,action) = ([(player,action):okTackleActions ++ otherActions],Just player)
											     nope                 = (                 okTackleActions ++ otherActions ,lastContact)
	= (okActions,{match & seed=seed, lastContact=newContact})
	where
		all_players							= team1 ++ team2
		ball								= getFootball theBall all_players
		groundVictims						= [playerID \\ {playerID,effect=Just (OnTheGround frames)} <- all_players | frames >= 0]
		
	/*	selectBallAction picks at most one action of {GainBall, KickBall, HeadBall, CatchBall} intentions.
		The association list is assumed to contain only these actions.
	*/	selectBallAction :: !(AssocList FootballerID FootballerAction) !RandomSeed -> (!Maybe (FootballerID,FootballerAction),!RandomSeed)
		selectBallAction intendedActions seed
		# (ps,seed)							= iterateStn (length intendedActions) nextRandomP seed
		= selectMostProbableAction [ (successOfAction action (if (p==one) p (makeRandomRealistic p)),action) \\ action <- intendedActions & p <- ps ] seed
		where
			successOfAction :: !(!FootballerID,!FootballerAction) !P -> P
			successOfAction (who,action) p	= me.stamina * me.health * p * success_of_action
			where
				success_of_action			= if (isGainBall  action && ballGainable  && ballAtGainSpeed)  success_gaining
											 (if (isCatchBall action && ballCatchable && ballAtCatchSpeed) success_catching
											 (if (isKickBall  action && ballKickable)                      success_kicking
											 (if (isHeadBall  action && ballHeadable)                      success_heading
											 	                                                           zero
											 )))
				me							= find1 (identify_player who) all_players
				mySkills 					= skillsAsList me
				length						= me.length
				iGainWell					= isMember Gaining  mySkills
				iKickWell					= isMember Kicking  mySkills
				iHeadWell					= isMember Heading  mySkills
				iCatchWell					= isMember Catching mySkills
				ballGainable				= d_player_ball <= maxGainReach  me && ball_height <= scale 0.8 length + scale (if iGainWell  0.2 0.0) length
				ballKickable				= d_player_ball <= maxKickReach  me && ball_height <= scale 0.4 length + scale (if iKickWell  0.6 0.0) length
				ballCatchable				= d_player_ball <= maxCatchReach me && ball_height <=           length + scale (if iCatchWell 1.0 0.5) length
				ballHeadable				= d_player_ball <= maxHeadReach  me && ball_height <=           length + scale (if iHeadWell  0.5 0.0) length && ball_height >= scale 0.8 length
				ballAtGainSpeed				= d_velocity    <= maxGainVelocityDifference  me d_player_ball
				ballAtCatchSpeed			= d_velocity    <= maxCatchVelocityDifference me d_player_ball
				d_speed 					= {zero & dxy = scale (toReal me.speed.velocity) (toRVector me.speed.direction)}
												-
											  {dxy = scale (toReal ball.ballSpeed.vxy.velocity) (toRVector ball.ballSpeed.vxy.direction),dz = m (toReal ball.ballSpeed.vz)}
				d_velocity 					= ms (toReal (size_vector3D d_speed))
				ball_height					= ball.ballPos.pz
				d_player_ball				= dist me ball
				others_with_ball			= case theBall of
											    GainedBy playerID = if (playerID <> who) (filter (identify_player playerID) all_players) []
											    free              = []
				other_has_ball				= not (isEmpty others_with_ball)
				otherDribblesWell			= isMember Dribbling (skillsAsList (hd others_with_ball))
				success_gaining				= if (ballIsFree theBall)   (if iGainWell 0.95 0.8)
											 (if  other_has_ball        (if iGainWell 0.75 0.3 * if otherDribblesWell 0.6 1.0)
												                        1.0)
				success_kicking				= if (ballIsFree theBall)   (if iKickWell 0.95 0.85)
											 (if  other_has_ball        (if iKickWell 0.80 0.70 * if otherDribblesWell 0.7 1.0)
												                        1.0)
				success_heading				= if iHeadWell  0.95 0.90
				success_catching			= if iCatchWell 1.00 0.95
		
	/**	selectTackleActions removes impossible tackle actions and, by chance, ignores some of the possible tackle actions.
	*/	selectTackleActions :: !(AssocList FootballerID FootballerAction) !RandomSeed -> (!AssocList FootballerID FootballerAction,!RandomSeed)
		selectTackleActions performedActions seed
			= filterSt isPossibleTackle [action \\ action <- performedActions | isFootballerTackle (snd action)] seed
		where
			isPossibleTackle :: !(!FootballerID,!FootballerAction) !RandomSeed -> (!Bool,!RandomSeed)
			isPossibleTackle (playerID,Tackle victimID _) seed
			| d_me_victim > maxTackleReach offender								// victim is out of reach
											= (False,seed)
			# (p,seed)						= nextRandomP seed
			| otherwise						= (avg [p,chanceOfSuccess] > 0.5,seed)	// victim is within reach, but tackle may fail
			where
				offender					= find1 (identify_player playerID) all_players
				victim						= find1 (identify_player victimID) all_players
				d_me_victim					= dist offender victim
				chanceOfSuccess				= avg [1.0 - toReal d_me_victim, if (isMember Tackling (skillsAsList offender)) 0.9 0.7]
		
/*	doFootballerActions intendedActions okActions match
		performs for each football player in match his succeededAction. 
*/	doFootballerActions :: !(AssocList FootballerID FootballerAction) !(AssocList FootballerID FootballerAction) !Match -> Match
	doFootballerActions intendedActions okActions match=:{theField,theBall,team1,team2,seed,nextRandomP}
	# (seed,ball,new_players1,new_players2)	= foldl (flip doAction) (seed,theBall,team1,team2) intendedActions
	= { match & team1 = new_players1, team2 = new_players2, theBall = ball, seed = seed }
	where
		dt									= toReal match.Match.unittime			// duration, in seconds, of one step
		{fwidth,flength}					= theField
		
		doAction :: !(!FootballerID,!FootballerAction) !(!RandomSeed,!FootballState,![Footballer],![Footballer]) 
		                                             -> (!RandomSeed,!FootballState,![Footballer],![Footballer])
		doAction intendedAction (seed,ball,allPlayers1,allPlayers2)
		| isMember intendedAction okActions	= act intendedAction (seed,ball,allPlayers1,allPlayers2)
		| otherwise							= (seed,ball,map (failThisPlayerAction intendedAction) allPlayers1,map (failThisPlayerAction intendedAction) allPlayers2)						
		where
			failThisPlayerAction :: !(!FootballerID,!FootballerAction) !Footballer -> Footballer
			failThisPlayerAction (id,idea) fb=:{playerID,effect}
			| id <> playerID				= fb
			| otherwise						= {fb & effect = new_effect}
			where
				new_effect					= case effect of
												Just (OnTheGround nr_of_frames)	= if (nr_of_frames < 0) Nothing (Just (OnTheGround (nr_of_frames-1)))
												_								= Just (failFootballerAction idea)
			
			act :: !(!FootballerID,!FootballerAction) !(!RandomSeed,!FootballState,![Footballer],![Footballer]) 
							                        -> (!RandomSeed,!FootballState,![Footballer],![Footballer])
			
		/** Rules for moving:
		*/	act (playerID,Move speed angle) (seed,ball,team1,team2)
			# (team1,team2)					= splitAt (length team1) (unbreak1 (uneq1,new_fb,uneq2))
			= (seed,ball,team1,team2)
			where
				(uneq1,fb,uneq2)			= break1 (identify_player playerID) (team1 ++ team2)
				feasible_angle				= scale (fromInt (sign angle)) (setbetween (abs angle) zero (maxRotateAngle fb))
				new_nose					= fb.nose + feasible_angle
				angleDifficulty 			= angleHowFarFromPi   (speed.direction-new_nose)
				angleDifference				= angleHowFarFromAngle speed.direction new_nose
				new_stamina					= alter_stamina ball fb angleDifficulty angleDifference
				new_vel						= scale (fb.health * new_stamina) (setbetween speed.velocity zero (maxVelocity (skillsAsList fb) angleDifficulty angleDifference))
				new_speed					= {speed & velocity=new_vel}
				new_position`				= move_point (scale (dt * (toReal new_vel)) (toRVector new_speed.direction)) fb.pos
				new_position				= point_to_rectangle ({px=scale -0.5 flength, py=scale -0.5 fwidth},{px=scale 0.5 flength,py=scale 0.5 fwidth}) new_position` 
				new_fb						= {fb & stamina = new_stamina
											      , speed   = new_speed
											      , pos     = new_position
											      , nose    = new_nose
											      , effect  = Just (Moved new_speed feasible_angle)
											  }
	
		/**	Rules for gaining ball:
			(1) ball obtains position and surface speed of obtaining player
		*/	act (playerID,GainBall) (seed,ball,team1,team2)
			# (team1,team2)					= splitAt (length team1) (unbreak1 (uneq1,new_fb,uneq2))
			= (seed,GainedBy playerID,team1,team2)
			where
				(uneq1,fb,uneq2)			= break1 (identify_player playerID) (team1 ++ team2)
				new_fb						= {fb & effect = Just (GainedBall Success)}
			
		/**	Rules for kicking ball:
			(1) kicking decreases stamina
			(2) kicking is more effective towards your direction, and least effective in opposite direction
			(3) being taller, you can kick harder
			(4) a low stamina/health lower your max kickspeed
			(5) todo: kicking a ball held/gained by a keeper, may damage the keeper
		*/	act (playerID,KickBall {vxy={velocity=v,direction=d},vz}) (seed,ball,team1,team2)
			# (team1,team2)					= splitAt (length team1) (unbreak1 (uneq1,new_fb,uneq2))
			= (seed1,Free new_ball,team1,team2)
			where
				(uneq1,fb,uneq2)			= break1 (identify_player playerID) (team1 ++ team2)
				new_fb						= {fb & stamina=new_stamina,effect=Just (KickedBall (Just new_speed))}
				theBall						= getFootball ball (team1 ++ team2)
				skills						= skillsAsList fb
				max_v						= maxVelocityBallKick fb
				new_v						= scale speed_factor (setbetween v  zero max_v)
				new_vz						= scale speed_factor (setbetween vz zero max_v)
				new_speed					= {vxy={velocity=new_v,direction=new_d},vz=new_vz}
				new_stamina					= kickingPenalty fb new_v * fb.stamina
				speed_factor				= oppositeKickPenalty fb d
				new_ball					= {theBall & ballSpeed=new_speed}
				(new_d,seed1)				= new_ball_direction Kicking fb d seed
			
		/**	Rules for heading ball:
			(1) heading decreases stamina, but less than kicking
			(2) kicking is more effective towards your direction, and least effective in opposite direction
			(3) a low stamina/health lower your max headspeed, but less than kicking
			(4) heading is less harder than kicking, but is not effected by your length
			(5) todo: heading a ball held/gained by a keeper, may damage the keeper (less than with kicking)
		*/	act (playerID,HeadBall {vxy={velocity=v,direction=d},vz}) (seed,ballstate,team1,team2)
			# (team1,team2)					= splitAt (length team1) (unbreak1 (uneq1,new_fb,uneq2))
			= (seed1,Free new_ball,team1,team2)
			where
				(uneq1,fb,uneq2)			= break1 (identify_player playerID) (team1 ++ team2)
				skills						= skillsAsList fb
				ball						= getFootball ballstate (team1 ++ team2)
				ball_speed					= ball.ballSpeed.vxy.velocity
				max_v						= maxVelocityBallHead fb ball_speed
				new_v						= setbetween v zero max_v
				new_vz						= scale 0.25 (setbetween vz zero max_v)
				new_speed					= {vxy={velocity=new_v,direction=new_d},vz=new_vz}
				new_stamina					= headingPenalty fb new_v ball_speed * fb.stamina
				new_fb						= {fb & stamina=new_stamina,effect=Just (HeadedBall (Just new_speed))}
				new_ball					= {ball & ballSpeed=new_speed}
				(new_d,seed1)				= new_ball_direction Heading fb d seed
			
		/**	Rules for feinting:
			(1) you must have velocity in order to feint manouvre.
			(2) a feint manouvre changes your position, and decreases your velocity (depends on Feinting skill)
		*/	act (playerID,Feint d) (seed,ball,team1,team2)
			# (team1,team2)					= splitAt (length team1) (unbreak1 (uneq1,new_fb,uneq2))
			= (seed,ball,team1,team2)
			where
				(uneq1,fb,uneq2)			= break1 (identify_player playerID) (team1 ++ team2)
				new_stamina					= (maxFatigueLossAtFeint fb) * fb.stamina
				new_velocity				= scale (fb.health * fb.stamina * (maxVelocityLossAtFeint fb)) fb.speed.velocity
				new_speed					= {fb.speed & velocity=new_velocity}
				(leftv,rightv)				= orthogonal fb.speed.direction
				sidestep					= case d of FeintLeft -> leftv; _ -> rightv
				new_position`				= move_point ((scale (toReal (maxFeintStep fb)) (toRVector sidestep))
				                    		                             + 
				                    		              (scale (dt * toReal new_velocity) (toRVector fb.speed.direction))
				                    		             ) fb.pos
				new_position				= point_to_rectangle ({px=scale -0.5 flength,py=scale -0.5 fwidth},{px=scale 0.5 flength,py=scale 0.5 fwidth}) new_position`
				new_fb						= {fb & pos=new_position,speed=new_speed,stamina=new_stamina,effect=Just (Feinted d)}
			
		/** Rules for Tackling
			(1) tackling may lower the health of the victim but increases his stamina (last is because he lies on the ground the next rounds)
			(2) tackling costs stamina
		*/	act (playerID,Tackle victimID ve) (seed,ball,team1,team2)
			= (seed1,new_ball,team1T,team2T)
			where
				nrPlayersTeam1				= length team1
				(uneq1,fb,uneq2)			= break1 (identify_player playerID) (team1 ++ team2)
				(team1N,team2N)				= splitAt nrPlayersTeam1 (unbreak1 (uneq1,new_fb,uneq2))
				(uneq1T,fbT,uneq2T)			= break1 (identify_player victimID) (team1N ++ team2N)
				(team1T,team2T)				= splitAt nrPlayersTeam1 (unbreak1 (uneq1T,new_target,uneq2T))
				new_stamina_self			= maxFatigueLossAtTackle fb * fb.stamina
				new_fb						= {fb & stamina = new_stamina_self, effect = Just (Tackled victimID ve Success)}
				target_has_ball				= ballIsGainedBy victimID ball
				(p,seed1)					= nextRandomP seed
				new_v`						= min max_tackle_velocity ve
				max_tackle_velocity			= ms 10.0
				max_ground_time				= s 30.0
				ground_frames				= toInt ((((toReal new_v`) / (toReal max_tackle_velocity)) * (toReal max_ground_time)) / dt)
				new_v						= scale 0.1 new_v`
				healthDamageTarget			= (toReal new_v) * fb.health * fb.stamina * (0.5*p + 0.1) + (toReal (fbT.length-min_length))/2.0
				new_health_target			= max zero (fbT.health - healthDamageTarget)
				new_target					= {fbT & health = new_health_target, effect = Just (OnTheGround ground_frames) }
				new_ball					= if target_has_ball (Free (mkFootball fbT.pos fbT.speed)) ball	
				
		/** Rules for catching
			(1) ball optains speed and distance of player
		*/	act (playerID,CatchBall) (seed,ball,team1,team2)
			# (team1,team2)					= splitAt (length team1) (unbreak1 (uneq1,new_fb,uneq2))
			= (seed,GainedBy playerID,team1,team2)
			where
				(uneq1,fb,uneq2)			= break1 (identify_player playerID) (team1 ++ team2)
				new_fb						= {fb & effect=Just (CaughtBall Success)}

			new_ball_direction :: !Skill !Footballer !Angle !RandomSeed -> (!Angle,!RandomSeed)
			new_ball_direction skill fb d seed
			# (p1,seed)						= nextRandomP seed
			# (p2,seed)						= nextRandomP seed
			| p2 == one						= (d,seed)
			# failure						= one - if (isMember skill (skillsAsList fb)) makeRandomRealisticSkilled makeRandomRealistic p2
			# diff							= scale failure (maxHeadingDeviation fb)
			| p1 <= 0.5						= (d - diff, seed)
			| otherwise						= (d + diff, seed)

/**	moveFootball match
		makes the free ball move (a gained ball moves along with its player).
*/	moveFootball :: !Match -> Match
	moveFootball match=:{Match | theBall=Free football=:{ballSpeed={vxy={velocity=v,direction=d},vz},ballPos},theField,team1,team2,seed,lastContact,unittime}
		= { match & theBall = Free {football & ballSpeed=new_speed,ballPos=new_ballpos}, seed = seed1, lastContact = if (isJust hit_player) hit_player lastContact }
	where
		old_height							= ballPos.pz
		in_the_air							= old_height > zero
		resistance							= rateAdjust if in_the_air air_resistance surface_resistance
		dt									= toReal unittime
		surface_movement					= scale (dt * (toReal v)) (toRVector d)
		new_speed2D							= let new_v = scale resistance v in {direction = d, velocity = if (new_v <= ms 0.05) zero new_v}
		new_vz`								= if in_the_air (vz - scale dt accelleration_sec) zero
		new_height`							= ballPos.pz + m (toReal vz)
		(new_height,new_vz)					= if (in_the_air && new_height` <= zero) // the ball bounces on the field
											     (scale 0.5 (abs new_height`),let new_vz`` = scale 0.33 (abs new_vz`) in if (new_vz`` <= ms 0.8) zero new_vz``)
											     (new_height`, new_vz`)
		new_speed`							= {vxy=new_speed2D, vz=new_vz}
		new_ballpos							= {pxy=move_point surface_movement ballPos.pxy,pz=new_height}
		all_players							= team1 ++ team2
		(hit_player,new_speed,seed1)		= ballBounces new_ballpos new_speed` seed
		
	//	the direction of the ball changes after a bounce and its velocity may reduce in case of bouncing against a player
		ballBounces :: !Position3D !Speed3D !RandomSeed -> (!Maybe FootballerID,!Speed3D,!RandomSeed)
		ballBounces new_ballpos new_speed=:{vxy={velocity=v,direction=d},vz=s3d} seed
		| hit_west_goal						= (Nothing,{new_speed & vxy = {new_speed.vxy & direction = if (d <= rad pi) (d - rad (0.5*pi)) (d + rad (0.5*pi)), velocity = v}},seed)
		| hit_east_goal						= (Nothing,{new_speed & vxy = {new_speed.vxy & direction = if (d <= rad pi) (d + rad (0.5*pi)) (d - rad (0.5*pi)), velocity = v}},seed)
		| isEmpty hit_players				= (Nothing, new_speed, seed)
		# (p1,seed)							= nextRandomP seed
		# (p2,seed)							= nextRandomP seed
		# (p3,seed)							= nextRandomP seed
		| otherwise							= (Just (hd hit_players),{vxy = {direction = rad (p2*2.0*pi), velocity = scale p3 v}, vz=scale p1 s3d},seed)
		where
			half_length						= scale 0.5 theField.flength
			goal_pole_r						= scale 0.5 goal_pole_width
			(northPole,southPole)			= goal_poles theField
			hit_west_goal					= againstGoalWestNorthPole || againstGoalWestSouthPole || againstGoalWestPoleUpper
			hit_east_goal					= againstGoalEastNorthPole || againstGoalEastSouthPole || againstGoalEastPoleUpper
			hit_players						= [playerID \\ fb=:{length,playerID} <- all_players | inRadiusOfFootballer new_ballpos.pxy fb && length >= new_ballpos.pz]
			againstGoalWestNorthPole	 	= inCircleRadiusOfPosition new_ballpos goal_pole_r goal_height {px = ~half_length, py = northPole + goal_pole_r}
			againstGoalWestSouthPole		= inCircleRadiusOfPosition new_ballpos goal_pole_r goal_height {px = ~half_length, py = southPole - goal_pole_r}
			againstGoalEastNorthPole		= inCircleRadiusOfPosition new_ballpos goal_pole_r goal_height {px =  half_length, py = northPole + goal_pole_r}
			againstGoalEastSouthPole		= inCircleRadiusOfPosition new_ballpos goal_pole_r goal_height {px =  half_length, py = southPole - goal_pole_r}
			againstGoalWestPoleUpper		= (isbetween new_ballpos.pxy.py (southPole - goal_pole_r) (northPole + goal_pole_r))
													&&
											  (isbetween new_ballpos.pz goal_height (goal_height+goal_pole_width))
													&&
											  (new_ballpos.pxy.px <= ~half_length)
			againstGoalEastPoleUpper		= (isbetween new_ballpos.pxy.py (southPole - goal_pole_r) (northPole + goal_pole_r))
													&&
											  (isbetween new_ballpos.pz goal_height (goal_height+goal_pole_width))
													&&
											  (new_ballpos.pxy.px >= half_length)
			inCircleRadiusOfPosition {pxy,pz} r zr pos
											= dist pxy pos <= r && pz <= zr

	moveFootball match
		= match
			
/**	advanceTime match
		decreases the time to play with unittime.
*/	advanceTime :: !Match -> Match
	advanceTime match=:{Match | playingTime, unittime}
		= {Match | match & playingTime = max zero (playingTime - minutes unittime)}

/*	Attribute altering functions depending on angles:
	params: 
		Angle :: between zero and pi, how much the player is running backwards (pi is backwards).
		Angle :: between zero and pi, the difference between the desired angle and the angle the player previously ran to.
*/
alter_stamina :: !FootballState !Footballer !Angle !Angle -> Stamina
alter_stamina ballState fb angleDifficulty angleDifference
| velocity <= rfv											// increase stamina
	| stamina < MinimumFatigue				= MinimumFatigue
	| otherwise								= stamina^(rateAdjust 0.8)
| otherwise									= fatigue * (rateAdjust factor)
where
	velocity								= fb.speed.velocity
	length									= fb.length
	stamina									= fb.stamina
	rfv										= restore_stamina_velocity (ballIsGainedBy fb.playerID ballState) (skillsAsList fb) angleDifficulty angleDifference
	diff									= velocity - rfv
	fv										= if (diff >= ms 6.0) (stamina^rateAdjust (stamina^(1.6 + 0.02 * toReal length)))  // problematic
											 (if (diff >= ms 4.0) (stamina^rateAdjust (         1.5 + 0.01 * toReal length))
											 (if (diff >= ms 2.0) (stamina^rateAdjust (         1.4 - 0.01 * toReal length))
											                      (stamina^rateAdjust (         1.3 - 0.02 * toReal length))))
	factor									= one - (toReal angleDifficulty)/(4.0*pi)
	fatigue									= if (stamina > MaximumFatigue) MaximumFatigue fv

restore_stamina_velocity :: !Bool ![Skill] !Angle !Angle -> Velocity
restore_stamina_velocity gained_ball skills angleDifficulty angleDifference
| gained_ball								= scale ( one / if (isMember Running   skills) 1.6 2.6)         max_v
| isMember Running skills					= scale ((one / if (isMember Dribbling skills) 2.0 3.0) * 1.22) max_v
| otherwise									= scale ( one / if (isMember Dribbling skills) 2.0 3.0)         max_v
where
	max_v									= maxVelocity skills angleDifficulty angleDifference

maxVelocity :: ![Skill] !Angle !Angle -> Velocity
maxVelocity skills angleDifficulty angleDifference
	= scale (dribblingPenalty * runningPenalty) base_velocity
where
	base_velocity							= ms 10.0
	dribblingPenalty						= if (isMember Dribbling skills) 0.95 0.85
	runningPenalty							= if (isMember Running   skills) 1.0  0.85

MinimumFatigue								:== 0.05
MaximumFatigue								:== 0.985


/**	The functions below defines the penalty factor: values between 0.0 and 1.0 that define the loss of an attribute of an action.
*/
::	PenaltyFactor :== Real					// a value between 0.0 and 1.0

kickingPenalty :: !Footballer !Velocity -> PenaltyFactor
kickingPenalty fb new_v						= 1.0 - (if (isMember Kicking (skillsAsList fb)) 0.3 0.6) * ((toReal new_v)/(toReal max_v))^2.0
where
	max_v									= maxVelocityBallKick fb

headingPenalty :: !Footballer !Velocity !Velocity -> PenaltyFactor
headingPenalty fb new_v ball_v				= 1.0 - (if (isMember Heading (skillsAsList fb)) 0.08 0.13) * ((toReal new_v)/(toReal max_v))^2.0
where
	max_v									= maxVelocityBallHead fb ball_v

maxFatigueLossAtTackle :: !Footballer -> PenaltyFactor
maxFatigueLossAtTackle fb					= if (isMember Tackling (skillsAsList fb)) 0.99 0.9

maxFatigueLossAtFeint :: !Footballer -> PenaltyFactor
maxFatigueLossAtFeint fb					= if (isMember Feinting (skillsAsList fb)) 0.92 0.77

maxVelocityLossAtFeint :: !Footballer -> PenaltyFactor
maxVelocityLossAtFeint fb					= if (isMember Feinting (skillsAsList fb)) 0.99 0.75

oppositeKickPenalty :: !Footballer !Angle -> PenaltyFactor
oppositeKickPenalty fb kick_to				= 1.0 - toReal (scale (skillPenaltyFactor/pi) (angleHowFarFromPi angle))
where
	angle									= abs (fb.nose - kick_to)
	skills									= skillsAsList fb
	skillPenaltyFactor						= if (isAllMember [Rotating,Kicking] skills) 0.3
											 (if (isAnyMember [Rotating,Kicking] skills) 0.5
									    	                                             0.9)

angleHowFarFromPi :: !Angle -> Angle
angleHowFarFromPi a
| a` > rad pi								= rad (2.0*pi) - a`
| otherwise									= a`
where
	a`										= abs a

angleHowFarFromAngle :: !Angle !Angle -> Angle
angleHowFarFromAngle a b
| a` > b`
	| a` - b` > rad pi						= b` - a` + rad (2.0*pi)
	| otherwise								= a` - b`
| otherwise
	| b` - a` > rad pi						= a` - b` + rad (2.0*pi)
	| otherwise								= b` - a`
where
	a`										= abs a
	b`										= abs b
