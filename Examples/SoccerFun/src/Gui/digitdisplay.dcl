definition module digitdisplay

import StdControl, StdId

/*	A DigitDisplay displayId format size colour atts
		is a Controls instance that displays a positive integral number of maximal format digits
		in the given colour. Each digit has a given size.
	The following ControlAttribute-s are inherited:
		ControlPos:	the layout position of the digit display control
		ControlTip:	the tool tip text that must be displayed
	All other attributes are ignored.
*/

::	DigitDisplay ls pst
 =	DigitDisplay DigitDisplayId DigitFormat DigitSize Colour [ControlAttribute *(ls,pst)]
::	DigitDisplayId			// The identification value of the digit display
::	DigitFormat
 =	IntegerFormat !Int		// The number of digits
::	DigitSize				// The size of one digit
 :== Size

openDigitDisplayId :: !*env -> (!DigitDisplayId,!*env) | Ids env

instance Controls DigitDisplay

getDigitDisplayValue :: !DigitDisplayId      !(PSt .ps) -> (!Int,!PSt .ps)
setDigitDisplayValue :: !DigitDisplayId !Int !(PSt .ps) -> PSt .ps
