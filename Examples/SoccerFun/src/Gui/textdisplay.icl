implementation module textdisplay

import StdInt, StdList, StdMisc
import StdControl, StdControlAttribute, StdControlReceiver, StdId, StdPSt, StdReceiver

/**	A TextDisplay is similar to a TextControl. A TextControl displays text using
	system settings. A TextDisplay uses the ControlPen attribute to display text.
	In addition, the ControlSize can be set by the programmer. 
	The following ControlAttribute-s are inherited:
		ControlMinimumSize: the minimum size of the control in case of resizing
		ControlPen:         the pen settings used to display the text
		ControlPos:         the layout position of the text display control
		ControlResize:      resizing the control
		ControlTip:         the tool tip text that must be displayed
*/
::	TextDisplay ls pst
 =	TextDisplay TextDisplayId String TextSize [ControlAttribute *(ls,pst)]
::	TextDisplayId
 =	{	customId	:: !Id
 	,	receiverId	:: !R2Id TextDisplayMsgIn TextDisplayMsgOut
 	}
::	TextSize :== Size	// The size of the text display
::	TextDisplayMsgIn  = SetTextIn String | GetTextIn
::	TextDisplayMsgOut = SetTextOut       | GetTextOut String

openTextDisplayId :: !*env -> (!TextDisplayId,!*env) | Ids env
openTextDisplayId env
	# (id,  env) = openId env
	# (r2id,env) = openR2Id env
	= ({customId=id,receiverId=r2id},env)

::	TextDisplaySt
 =	{	text	:: !String	// The text to be displayed
 	}

instance Controls TextDisplay where
	controlToHandles (TextDisplay {customId,receiverId} text size atts) pst
		= controlToHandles textdisplay pst
	where
		customControlSize	= validateTextDisplaySize minSize size
		initDisplaySt		= { text	= text
							  }
		textdisplay			= { newLS	= initDisplaySt
							  , newDef	= CustomControl customControlSize (displaylook text) [ ControlId customId : okAtts ]
											:+:
										  Receiver2 receiverId receiverfun []
							  }
		okAtts				= map toTextDisplayAttribute (filter isTextDisplayAttribute atts)
		minSize				= case filter isControlMinimumSize okAtts of
								[ControlMinimumSize s : _]	-> s
								_							-> zero

		receiverfun :: !TextDisplayMsgIn !*(TextDisplaySt,PSt .ps) -> (TextDisplayMsgOut,(TextDisplaySt,PSt .ps))
		receiverfun (SetTextIn str) (tdst,pst=:{io})
			# tdst	= {tdst & text = str}
			# io	= setControlLook customId True (True,displaylook str) io
			= (SetTextOut,(tdst,{pst & io=io}))
		receiverfun GetTextIn (tdst=:{text},pst)
			= (GetTextOut text,(tdst,pst))
		
		displaylook :: !String !SelectState !UpdateState !*Picture -> *Picture
		displaylook text _ {newFrame} picture
			# picture			= unfill newFrame picture
			# (metrics,picture)	= getPenFontMetrics picture
			# base				= metrics.fAscent + metrics.fLeading
			# height			= fontLineHeight metrics
			# (width,picture)	= getPenFontStringWidth text picture
			# picture			= drawAt {x=max 0 ((w-width)/2),y=(h-height)/2+base} text picture
			= picture
		where
			{w,h}				= rectangleSize newFrame
		
	getControlType _ = "TextDisplay"

isTextDisplayAttribute :: !(ControlAttribute .ps) -> Bool
isTextDisplayAttribute (ControlMinimumSize _)	= True
isTextDisplayAttribute (ControlPen         _)	= True
isTextDisplayAttribute (ControlPos         _)	= True
isTextDisplayAttribute (ControlResize      _)	= True
isTextDisplayAttribute (ControlTip         _)	= True
isTextDisplayAttribute _						= False

toTextDisplayAttribute :: !(ControlAttribute *(.ls,.pst)) -> ControlAttribute *(TextDisplaySt,.pst)
toTextDisplayAttribute (ControlMinimumSize s)	= ControlMinimumSize s
toTextDisplayAttribute (ControlPen         p)	= ControlPen         p
toTextDisplayAttribute (ControlPos         p)	= ControlPos         p
toTextDisplayAttribute (ControlResize      f)	= ControlResize      f
toTextDisplayAttribute (ControlTip         t)	= ControlTip         t


validateTextDisplaySize :: !Size !Size -> Size
validateTextDisplaySize minSize {w,h} = {w=max w (min 0 minSize.w),h=max h (min 0 minSize.h)}

getTextDisplayText :: !TextDisplayId !(PSt .ps) -> (!String,!PSt .ps)
getTextDisplayText {receiverId} pst
	= case syncSend2 receiverId GetTextIn pst of
		((SendOk,Just (GetTextOut x)),pst) -> (x,pst)
		other                              -> sendError "getTextDisplayText"

setTextDisplayText :: !TextDisplayId !String !(PSt .ps) -> PSt .ps
setTextDisplayText {receiverId} newTxt pst
	= case syncSend2 receiverId (SetTextIn newTxt) pst of
		((SendOk,Just SetTextOut),pst) -> pst
		other                          -> sendError "setTextDisplayText"

sendError fName
	= abort (fName +++ ": wrong reply from TextDisplay.\n")
