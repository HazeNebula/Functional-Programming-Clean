module Aligning

/*  Main module for aligning text.

	Peter Achten.
	Translation: Thom Wiggers
*/
import StdEnv, StdIO
import StdDebug
import Group

//  Some type synonyms
::  Text		 :== String
::  Word		 :== String
::  Line		 :== String
::  TextWidth	 :== Int
::  LetterWidth  :== Int
::  Space		 :== String   // The whitespace between two words. Length > 0, all characters are ' '.

//  AlignMode represents the options of the algorithm.
::	AlignMode = TextAlignLeft | TextAlignCentred | TextAlignRight | TextAlignJustified

instance == AlignMode where
	== TextAlignLeft      TextAlignLeft      = True
	== TextAlignCentred   TextAlignCentred   = True
	== TextAlignRight     TextAlignRight     = True
	== TextAlignJustified TextAlignJustified = True
	== _                  _                  = False
instance toString AlignMode where
	toString TextAlignLeft      = "TextAlignLeft"
	toString TextAlignCentred   = "TextAlignCentred"
	toString TextAlignRight     = "TextAlignRight"
	toString TextAlignJustified = "TextAlignJustified"

/**
 * You need to write 'align' yourself.
 *  Arguments:
 *		AlignMode:		How to align the output (left, centred, right, justified)
 *		LetterWidth:	The width of a glyph (all glyphs are the same width)
 *		TextWidth:		The width of the output window
 *		Text:			The text to be aligned
 *	Output:
 *		[Line]:			The list of lines that need to be drawn below each other.
 *						The length of all lines needs to be equal.
 */

align :: AlignMode LetterWidth TextWidth Text -> [Line]
align _ _ _ _ = trace_n "align not yet implemented" []

/*
	From here on follows the supplied part of the programme that draws a window on your screen.
	You do not need to understand this part.
	We do cordially invite you to look at this programme if you are curious how it works.
*/

//  The data types used in the program:
::  FontInfo
 =  {   font	:: !Font
	,   width   :: !Int
	,   height  :: !Int
	}
::  State
 =  {   fontInfo:: !FontInfo
	,   mode	:: !AlignMode
	,   window  :: !Id
	}

/*  The text to be outlined. You can change this value to test your program. This example contains sufficient
	problems. ;-)
*/
text =: "In this short story some short words occur, such as 'a', 'b', 'c' and 'd'. " +++
	"However, there are also some exorbitantly long words such as 'supercalifragilisticexpialidocious'. " +++
	"Your algorithm should of course also be able to handle lines that contain such long words."

Start :: *World -> *World
Start world
# (fontInfo,world)  = accScreenPicture (getFontInfo 8) world
# (windowId,world)  = openId world
# initialState	  = {fontInfo=fontInfo,mode=TextAlignLeft,window=windowId}
= startIO SDI initialState initialiseGUI [ProcessClose closeProcess] world
where
//  initialiseGUI creates the window and menu of this interactive process.
	initialiseGUI :: (PSt State) -> PSt State
	initialiseGUI pst=:{ls=state}
	# (error,pst)	= openWindow undef wdef pst
	| error<>NoError= abort "Could not open the window."
	# (error,pst)	= openMenu   undef mdef pst
	| error<>NoError= abort "Could not open the menu."
	| otherwise		= pst
	where
	//  mdef defines the menu of the application.
		mdef		= Menu
						"&File"											// The menu title
						(												// The menu elements
							SubMenu "Font&Size"
						(   RadioMenu		[  (toString fsize,Nothing,Nothing,noLS (setFontSize fsize))
											 \\ fsize<-[8,10..30]
											 ]  0 []
						)   []
						:+: SubMenu "Mode"
						(   RadioMenu		[  (toString mode,Nothing,Nothing,noLS (setMode mode))
											 \\ mode<-[TextAlignLeft,TextAlignCentred,TextAlignRight,TextAlignJustified]
											 ]  0 []
						)   []
						:+: MenuSeparator	[]
						:+: MenuItem "E&xit" [MenuShortKey 'q',MenuFunction (noLS closeProcess)]
						)   []											// The menu has no attributes
		where
		//  setFontSize sets the new size of the current font and redraws everything.
			setFontSize :: Int (PSt State) -> PSt State
			setFontSize fsize pst=:{ls=state,io}
			# (maybe_info,io)   = accWindowPicture window (getFontInfo fsize) io
			| isNothing maybe_info
				= abort ("Could not retrieve font information of size "+++toString fsize)
			| otherwise
				# info		= fromJust maybe_info
				# state		= {state & fontInfo=fromJust maybe_info}
				# io		= appWindowPicture window (setPenFont info.font) io
				# io		= setWindowLook window True (False,look state) io
				= {pst & ls=state,io=io}
			where
				window	  = state.window

		//  setMode sets the new AlignMode and redraws everything.
			setMode :: AlignMode (PSt State) -> PSt State
			setMode mode pst=:{ls=state,io}
			# state			= {state & mode=mode}
			# io			= setWindowLook window True (False,look state) io
			= {pst & ls=state,io=io}
			where
				window	  = state.window

	//  wdef defines the window that displays the text.
		wdef		= Window
						"Aligning"										// The window title
						NilLS											// The window contains no controls
						[												// Window attributes:
							WindowClose	(noLS closeProcess)				// Closing the window terminates the program
						,   WindowLook	False (look state)				// The visible content of the window
						,   WindowId	state.window					// The identification value of the window
						,   WindowInit	(noLS (appPIO (appWindowPicture state.window (setPenFont state.fontInfo.font))))
																		// Set a non-proportional font
						,   WindowViewSize {w=300,h=300}
						]

//  getFontInfo retrieves the font metrics and non-proportional font.
	getFontInfo :: Int *Picture -> (FontInfo,*Picture)
	getFontInfo fsize picture
	# ((ok,font),picture)   = openFont {NonProportionalFontDef & fSize=fsize} picture
	| not ok
		= abort "Could not retrieve NonProportionalFontDef from screen picture"
	| otherwise
		# (metrics,picture) = getFontMetrics font picture
		= ({font=font,width=metrics.fMaxWidth,height=fontLineHeight metrics},picture)

//  look draws the visible content of the window.
	look :: State SelectState UpdateState *Picture -> *Picture
	look {fontInfo,mode} _ {oldFrame,newFrame} picture
	# picture		= unfill newFrame picture
	# picture		= setPenColour Red picture
	# picture		= drawAt {zero & x=framewidth/charwidth*charwidth} {zero & vy=frameheight} picture
	# picture		= setPenColour Black picture
	= seq [  drawAt {x=i*charwidth,y=y} line.[i]
		  \\ line<-linedtext & y<-[lineheight,2*lineheight..], i<-[0..size line-1]
		  ]  picture
	where
		linedtext   = align mode charwidth framewidth text
		framesize   = rectangleSize newFrame
		framewidth  = framesize.w
		frameheight = framesize.h
		lineheight  = fontInfo.height
		charwidth   = fontInfo.width
