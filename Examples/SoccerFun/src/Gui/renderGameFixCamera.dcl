definition module renderGameFixCamera

/** This module implements a camera at the south-east corner of the football field.
	The field is displayed in an isometric style.
	Because a 'pure' isometric projection gives the illusion that the west-edge of the football field
	is too wide, a visual correction is added to decrease this effect.
*/
import render

/** renderFixCamera
		renders a match using an elevated camera at the south-east corner of the football field
		and fixed angles.
*/
renderFixCamera :: RenderStyle
