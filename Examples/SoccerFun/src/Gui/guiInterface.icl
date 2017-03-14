implementation module guiInterface

import fileIO
import matchGame

inDir img :== "afbeeldingen\\"+++img+++".bmp"

getSplashImageForGui :: !*env -> (!GamePicture,!*env) | FileSystem env
getSplashImageForGui env
# pathToSplash		= inDir "AmsterdamArenA"
# (splashImg, env)	= getImage pathToSplash env
= ({img = splashImg, path = pathToSplash},env)

stepMatchForGui :: !FootballGame !*env -> (![RefereeAction], !FootballGame, !*env) | FileSystem env
stepMatchForGui game env
| timeLeft game
	# ((refEvents,actions),match)	= stepMatch game.match
	# env							= logMatch game.logging match refEvents actions env
	= (refEvents,{game & match=match},env)
| otherwise
	= ([GameOver],game,env)

getActionPicsForGui :: !Match !*env -> (!ActionPics,!*env) | FileSystem env
getActionPicsForGui match env
# (refereePics,env)	= sseqList (map fillWithPics` match.theReferee.refActionPics) env
= ({refereePics=refereePics},env)
where
	fillWithPics` :: !Path !*env -> (!GamePicture,!*env) | FileSystem env
	fillWithPics` path env
	# (img,env)			= getImage path env
	= ({img=img, path=path},env)

instance toString   DisplaySpeed where	toString Slowest		= "Slowest"
										toString Slower			= "Slower"
										toString Normal			= "Normal"
										toString Fast           = "Fast"
										toString Faster			= "Faster"
										toString Fastest		= "Fastest"
instance fromString DisplaySpeed where	fromString "Slowest"	= Slowest
										fromString "Slower"		= Slower
										fromString "Normal"		= Normal
										fromString "Fast"		= Fast
										fromString "Faster"		= Faster
										fromString "Fastest"	= Fastest
instance ==         DisplaySpeed where	== Slowest Slowest		= True
										== Slower  Slower		= True
										== Normal  Normal		= True
										== Fast    Fast			= True
										== Faster  Faster		= True
										== Fastest Fastest		= True
										== _       _			= False

intervalFactor :: !DisplaySpeed -> Real
intervalFactor Slowest		= 1.0 / 0.2	// five times slower
intervalFactor Slower		= 1.0 / 0.5	// two  times slower
intervalFactor Normal		= 1.0		// normal speed
intervalFactor Fast			= 1.0 / 2.0	// two times faster
intervalFactor Faster		= 1.0 / 5.0 // five times faster
intervalFactor Fastest		= zero		// no timer delay
