implementation module render

import StdIOCommon, StdPicture, StdPSt
import matchControl
import renderGameFlatlandish									// the default rendering style
import renderGameFixCamera										// 2.5D rendering with fixed camera and angles at south-east

instance nameOf RenderStyle	where nameOf style = style.RenderStyle.name

allRenderStyles				:: [RenderStyle]
allRenderStyles				= [ renderFlatland
							  , renderFixCamera
							  ]

green						:== RGB {r=30,g=140,b=40}			// dark green works better on a beamer
home_colour					:== RGB {r=255,g=255,b=70}			// yellow works well on dark green background
away_colour					:== RGB {r=102,g=255,b=255}

WestColour					:: Colour
WestColour					= home_colour

EastColour					:: Colour
EastColour					= away_colour

renderAttributes ::			!*env -> (![PenAttribute],!*env) | accScreenPicture env
renderAttributes env		= accScreenPicture attributes env
where
	attributes ::			!*Picture -> (![PenAttribute],!*Picture)
	attributes picture
		# ((_,font),picture)= openFont SmallFontDef picture
		= ([PenBack green,PenFont font],picture)
