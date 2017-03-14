implementation module RangeSlider

import StdEnv, StdIO

::	RangeSliderId a
	=	{ sliderId	:: Id
		, recId     :: R2Id (RangeSliderMsgIn a) (RangeSliderMsgOut a)
		}
::	RangeSliderMsgIn a
	=	RSIn_GetIndex
	|	RSIn_GetValue
	|	RSIn_SetIndex RangeIndex
	|	RSIn_SetValue a
::	RangeSliderMsgOut a
	=	RSOut_GetIndex RangeIndex
	|	RSOut_GetValue a
	|	RSOut_SetIndex
	|	RSOut_SetValue
::	RangeSliderSt a
	:== Range a

openRangeSliderId :: !*env -> (!RangeSliderId a,!*env) | Ids env
openRangeSliderId env
	# (sliderId,env)	= openId   env
	# (recId,   env)	= openR2Id env
	= ({sliderId=sliderId,recId=recId},env)

instance Controls (RangeSlider a) where
	controlToHandles (RangeSlider {sliderId,recId} dir width range=:{values,index} f atts) pSt
		= controlToHandles impl pSt
	where
		f`		= liftF2 f
		impl	= { addLS  = range
				  , addDef = SliderControl dir width state (action f`) [ControlId sliderId:map toLS atts]
								:+:
						     Receiver2 recId (recF f`) []
				  }
		state	= { sliderMin   = 0
				  , sliderMax   = length values - 1
				  , sliderThumb = index
				  }
		
		action f move ((range=:{values,index},ls),pSt)
				= f (values!!i) (({range & index=i},ls),appPIO (setSliderThumb sliderId i) pSt)
		where
			i	= case move of
					SliderIncSmall			= min (index+1)                          (length values-1)
					SliderDecSmall			= max (index-1)                          0
					SliderIncLarge			= min (index+(max 1 (length values)/10)) (length values-1)
					SliderDecLarge			= max (index-(max 1 (length values)/10)) 0
					SliderThumb new			= new
		
		recF f RSIn_GetIndex ((range=:{index},ls),pSt)
			= (RSOut_GetIndex index,((range,ls),pSt))
		recF f RSIn_GetValue ((range=:{values,index},ls),pSt)
			= (RSOut_GetValue (values!!index),((range,ls),pSt))
		recF f (RSIn_SetIndex i) ((range=:{values},ls),pSt)
			= (RSOut_SetIndex,f (values!!i`) (({range & index=i`},ls),pSt))
		where
			i`								= min (max i 0) (length values-1)
		recF f (RSIn_SetValue v) ((range=:{values,index},ls),pSt)
			= (RSOut_SetValue,f v (({range & values=updateAt index v values},ls),pSt))
		
		toLS (ControlActivate     f)		= ControlActivate   (liftF f)
		toLS (ControlDeactivate   f)		= ControlDeactivate (liftF f)
		toLS (ControlFunction     f)		= ControlFunction   (liftF f)
		toLS  ControlHide					= ControlHide
		toLS (ControlId           id)		= ControlId         id
		toLS (ControlKeyboard     kF s f)	= ControlKeyboard kF s (liftF2 f)
		toLS (ControlMinimumSize  s)		= ControlMinimumSize s
		toLS (ControlModsFunction f)		= ControlModsFunction  (liftF2 f)
		toLS (ControlMouse        mF s f)	= ControlMouse    mF s (liftF2 f)
		toLS (ControlPen          p)		= ControlPen         p
		toLS (ControlPos          p)		= ControlPos         p
		toLS (ControlResize       f)		= ControlResize      f
		toLS (ControlSelectState  s)		= ControlSelectState s
		toLS (ControlTip          t)		= ControlTip         t
		toLS (ControlWidth        w)		= ControlWidth       w
		toLS (ControlHMargin      l r)		= ControlHMargin     l r
		toLS (ControlHScroll      f)		= ControlHScroll     f
		toLS (ControlItemSpace    x y)		= ControlItemSpace   x y
		toLS (ControlLook         b f)		= ControlLook        b f
		toLS (ControlOrigin       o)		= ControlOrigin      o
		toLS (ControlOuterSize    s)		= ControlOuterSize   s
		toLS (ControlViewDomain   d)		= ControlViewDomain  d
		toLS (ControlViewSize     s)		= ControlViewSize    s
		toLS (ControlVMargin      t b)		= ControlVMargin     t b
		toLS (ControlVScroll      f)		= ControlVScroll     f
		
		liftF f ((add,ls),pSt)
			# (ls,pSt)	= f (ls,pSt)
			= ((add,ls),pSt)
		
		liftF2 f a ((add,ls),pSt)
			# (ls,pSt)	= f a (ls,pSt)
			= ((add,ls),pSt)
	
	getControlType _
		= "RangeSlider"

getRangeSliderIndex :: !(RangeSliderId a) !(PSt .ps) -> (!Maybe RangeIndex,!PSt .ps)
getRangeSliderIndex {recId} pSt
	= case syncSend2 recId RSIn_GetIndex pSt of
		((SendOk,Just (RSOut_GetIndex i)),pSt)	= (Just i, pSt)
		(_,pSt)									= (Nothing,pSt)

getRangeSliderValue :: !(RangeSliderId a) !(PSt .ps) -> (!Maybe a,!PSt .ps)
getRangeSliderValue {recId} pSt
	= case syncSend2 recId RSIn_GetValue pSt of
		((SendOk,Just (RSOut_GetValue v)),pSt)	= (Just v, pSt)
		(_,pSt)									= (Nothing,pSt)

setRangeSliderIndex :: !(RangeSliderId a) !Index !(PSt .ps) -> PSt .ps
setRangeSliderIndex {recId} i pSt
	= snd (syncSend2 recId (RSIn_SetIndex i) pSt)

setRangeSliderValue :: !(RangeSliderId a) !a !(PSt .ps) -> PSt .ps
setRangeSliderValue {recId} v pSt
	= snd (syncSend2 recId (RSIn_SetValue v) pSt)
