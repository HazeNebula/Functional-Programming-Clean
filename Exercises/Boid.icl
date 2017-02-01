module Boid

/*	
 *	This framework shows a collection of boids.
 *	For background information, see:
		http://www.red3d.com/cwr/boids/
		http://www.vergenet.net/~conrad/boids/pseudocode.html
	Set 'Environment' to 'Object IO' and 'Project Options' to 'No Console'.
*/
import StdEnv
import StdDebug
import StdIO, support

::	BoidSt
 =	{	boids		:: ![Boid]
 	,	colours		:: ![Colour]
 	}
::	Boid
 =	{	pos			:: !Pos
 	,	vel			:: !Vel
 	,	col			:: !Colour
 	}

Start				:: *World -> *World
Start world			= startIO SDI {boids=[],colours=flatten (repeat colours)} (openIds 2 `bind` initGUI) [ProcessClose closeProcess] world

initGUI				:: [Id] (PSt BoidSt) -> PSt BoidSt
initGUI [windowId,timerId] pSt=:{ls}
# (errors,pSt)		= seqList [openWindow NilLS window,openTimer undef timer,openMenu undef menu] pSt
| all ((==) NoError) errors
					= pSt
| otherwise			= abort "Could not create the entire GUI.\n"
where
	window			= Window "Boids" NilLS
						[ WindowClose			(noLS closeProcess)
						, WindowViewSize		windowsize
						, WindowLook			True (look ls.boids)
						, WindowMouse			onlyDown Able (noLS1 mouse)
						, WindowId				windowId
						, WindowPen				[PenBack background]
						]
	timer			= Timer (ticksPerSecond / 20) NilLS [TimerFunction (noLS1 (const lockstep)),TimerId timerId,TimerSelectState Unable]
	menu			= Menu "&File"
						(   MenuItem "&Clear"	[MenuShortKey 'c',MenuFunction (noLS clear)]
						:+: MenuSeparator		[]
						:+: MenuItem "S&tep"	[MenuShortKey 't',MenuFunction (noLS lockstep)]
						:+: MenuItem "&Go"		[MenuShortKey 'g',MenuFunction (noLS (appPIO (enableTimer  timerId)))]
						:+: MenuItem "&Halt"	[MenuShortKey 'h',MenuFunction (noLS (appPIO (disableTimer timerId)))]
						:+: MenuSeparator		[]
						:+: MenuItem "&Quit"	[MenuShortKey 'q',MenuFunction (noLS closeProcess)]
						)	[]
	onlyDown mst	= case mst of
						MouseDown _ _ _ = True
						_				= False

/*	look draws the boids
*/	look			:: [Boid] SelectState UpdateState *Picture -> *Picture
	look boids _ {newFrame} picture
					= foldr (\{pos,col} -> appPicture (fillAt (toPoint2 wsize pos) boidbody o setPenColour col)) (unfill newFrame picture) boids
	where
		wsize		= rectangleSize newFrame

/*	lockstep computes the next position for all droids
*/	lockstep pSt=:{ls}
		= appPIO (setWindowLook windowId True (True,look new_boids)) {pSt & ls={ls & boids = new_boids}}
	where
		new_boids	= simulate ls.boids

/*	clear removes
*/	clear pSt=:{ls}	= appPIO (setWindowLook windowId True (True,look [])) ({pSt & ls={ls & boids=[]}})

/*	mouse creates a new boid with speed (0,0) at the position of the cursor.
*/	mouse mSt pSt=:{ls=ls=:{boids,colours}}
					= case mSt of
						MouseDown pos _ _ 
							# (wsize,pSt)	= accPIO (getWindowViewFrame windowId) pSt
							# new			= {pos=fromPoint2 (rectangleSize wsize) pos,vel=zero,col=hd colours}
							# boids			= [new:boids]
							# pSt			= {pSt & ls={ls & boids=boids,colours=tl colours}}
							# pSt			= appPIO (setWindowLook windowId True (True,look boids)) pSt
							= pSt
						otherwise			= pSt

windowsize			= {w=640,h=400}	
colours				= [Red,Green,Blue,Cyan,Magenta,Yellow,LightGrey,Grey,DarkGrey]

boidbody			= circle 3
background			= Black
threshhold_dist		= 0.02
threshhold_wall		= 0.01
viewing_distance	= 0.3

/**
 * This is the function you have to write yourself.
 * Compute the next position of all boids in the list.
 */
simulate :: [Boid] -> [Boid]
simulate boids = trace_n "boids not yet implemented" boids

