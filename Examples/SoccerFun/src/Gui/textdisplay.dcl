definition module textdisplay

import StdControl, StdId

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
::	TextDisplayMsgIn  = SetTextIn String | GetTextIn
::	TextDisplayMsgOut = SetTextOut       | GetTextOut String
 
::	TextSize :== Size	// The size of the text display

openTextDisplayId :: !*env -> (!TextDisplayId,!*env) | Ids env

instance Controls TextDisplay

getTextDisplayText :: !TextDisplayId                 !(PSt .ps) -> (!String,!PSt .ps)
setTextDisplayText :: !TextDisplayId !String         !(PSt .ps) -> PSt .ps
