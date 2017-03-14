definition module RangeSlider

import StdEnv, StdIO

/**	A (RangeSlider id dir width range action atts) value describes a control that allows users to select values from the 
	given range. The parameters have the following meaning:
	id:		the identification value of the range slider;
	dir:	the direction of the range slider (Horizontal or Vertical);
	width:	the length of the range slider;
	range:	the range {values=[v_0, ..., v_n],index=i}; here i is the 0-based index of the currently selected value, v_i.
			When the user selects another value, then index will have the corresponding value;
	action:	the function that is applied to the current local and process state of the control whenever the user has used
			the range slider;
	atts:	the standard set of control attributes.
	
	Author: Peter Achten
	E-mail:	P.Achten@cs.ru.nl
	Date:	january 2008
	Uses:	Clean 2.2, Object I/O
*/
::	RangeSliderId a
::	RangeSlider a ls pst
	=	RangeSlider (RangeSliderId a) Direction ControlWidth (Range a) (RangeAction a *(ls,pst)) [ControlAttribute *(ls,pst)]
::	Range a
	=	{ values	:: ![a]
		, index		:: !RangeIndex
		}
::	RangeIndex
	:== Int
::	RangeAction a st
	:== a -> st -> st

openRangeSliderId :: !*env -> (!RangeSliderId a,!*env) | Ids env

instance Controls (RangeSlider a)

/** getRangeSliderIndex id pSt
		yields (Just index) of the selected value of the indicated range slider if it exists, and Nothing otherwise.
	getRangeSliderValue id pSt
		yields (Maybe (values!!index)) of the indicated range slider if it exists, and Nothing otherwise.
	setRangeSliderIndex id i pSt
		sets the current index value of the indicated range slider to i, and applies the action function to that value
		and the current local and process state.
		If the indicated range slider does not exist, nothing happens.
	setRangeSliderValue id v pSt
		updates the value at the current index of the indicated range slider to v, and applies the action function to 
		that value and the current local and process state.
		If the indicated range slider does not exist, nothing happens.
*/
getRangeSliderIndex :: !(RangeSliderId a)        !(PSt .ps) -> (!Maybe RangeIndex,!PSt .ps)
getRangeSliderValue :: !(RangeSliderId a)        !(PSt .ps) -> (!Maybe a,         !PSt .ps)
setRangeSliderIndex :: !(RangeSliderId a) !Index !(PSt .ps) ->                     PSt .ps
setRangeSliderValue :: !(RangeSliderId a) !a     !(PSt .ps) ->                     PSt .ps
