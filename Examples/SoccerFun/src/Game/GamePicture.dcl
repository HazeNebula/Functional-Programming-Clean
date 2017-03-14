definition module GamePicture

/**	Type definitions for handling bitmaps for referees (and footballers in the future).
*/

import StdBitmap

::	Path			:== String
::	GamePicture		=	{ img			:: !Bitmap
						, path			:: !Path
						}
::	ActionPics		=	{ refereePics	:: ![GamePicture]
						}
