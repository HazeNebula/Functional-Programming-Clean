definition module render

import StdIOCommon, StdPicture, StdPSt
import matchControl

::	RenderStyle		= { name :: !String
					  , look :: !Match -> Look
					  }

instance nameOf RenderStyle

/**	allRenderStyles:
		enumerates all present rendering styles.
*/
allRenderStyles		:: [RenderStyle]

/**	renderAttributes
		yields the attributes required by the rendering functions to accurately draw a match.
*/
renderAttributes	:: !*env -> (![PenAttribute],!*env) | accScreenPicture env

WestColour			:: Colour
EastColour			:: Colour
