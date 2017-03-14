implementation module digitdisplay

import StdArray, StdClass, StdEnum, StdInt, StdList, StdMisc, StdReal, StdString
import StdEnvExt
import StdControl, StdControlAttribute, StdControlReceiver, StdId, StdPicture, StdPSt, StdReceiver

/*	A DigitDisplay displayId format size atts
		is a Controls instance that displays a positive integral number of maximal format digits
		in a given colour. Each digit has a given size.
	The following ControlAttribute-s are inherited:
		ControlPos:	the layout position of the digit display control
		ControlTip:	the tool tip text that must be displayed
	All other attributes are ignored.
*/

::	DigitDisplay ls pst
 =	DigitDisplay DigitDisplayId DigitFormat DigitSize Colour [ControlAttribute *(ls,pst)]
::	DigitDisplayId
 =	{	customId	:: !Id
 	,	receiverId	:: !R2Id DigitDisplayMsgIn DigitDisplayMsgOut
 	}
::	DigitDisplayMsgIn  = SetDigitValueIn Int | GetDigitValueIn
::	DigitDisplayMsgOut = SetDigitValueOut    | GetDigitValueOut Int
::	DigitFormat
 =	IntegerFormat !Int		// The number of digits
::	DigitSize				// The size of one digit
 :== Size

openDigitDisplayId :: !*env -> (!DigitDisplayId,!*env) | Ids env
openDigitDisplayId env
	# (id,  env) = openId env
	# (r2id,env) = openR2Id env
	= ({customId=id,receiverId=r2id},env)

::	DigitDisplaySt
 =	{	value		:: !Int				// The current value that is displayed
 	}
::	DigitShapeTT
 =	{	horDigitBars:: ![RPoint2]		// The positions of the horizontal digitbars
 	,	verDigitBars:: ![RPoint2]		// The positions of the vertical   digitbars
 	}
::	DigitBarTT
 =	{	horDigitBar	:: !PolygonShapeTT	// The horizontal `true type' polygon shape
 	,	verDigitBar	:: !PolygonShapeTT	// The vertical   `true type' polygon shape
 	}
::	RPoint2			=	{rx ::!Real,ry ::!Real}
::	RVector2		=	{rvx::!Real,rvy::!Real}
::	PolygonShapeTT	:== [RVector2]


instance Controls DigitDisplay where
	controlToHandles (DigitDisplay {customId,receiverId} digitFormat digitSize colour atts) pst
		= controlToHandles digitdisplay pst
	where
		okDigitFormat		= validateDigitFormat digitFormat
		okDigitSize			= validateDigitSize digitSize
		nrOfDigits			= getNrOfDigits digitFormat
		customControlSize	= { okDigitSize & w = okDigitSize.w*nrOfDigits }
		initDisplaySt		= { value	= 0 }
		digitdisplay		= { newLS	= initDisplaySt
							  , newDef	= CustomControl customControlSize (displaylook (format okDigitFormat 0)) 
							  						[ ControlId customId
							  						, ControlPen [PenBack Black,PenColour colour]
							  						: flatten [posAtt,tipAtt]
							  						]
											:+:
										  Receiver2 receiverId receiverfun []
							  }
		
		posAtt				= case [att \\ att<-atts | isControlPos att] of
								[ ControlPos pos : _]	-> [ControlPos pos]
								_						-> []
		tipAtt				= case [att \\ att<-atts | isControlTip att] of
								[ ControlTip text : _]	-> [ControlTip text]
								_						-> []
		digitbars			= { horDigitBar	= [ {rvx = dx,    rvy = ~dy}
											  , {rvx = 1.0-lx,rvy = 0.0}
											  , {rvx = dx,    rvy = dy}
											  , {rvx = ~dx,   rvy = dy}
											  , {rvx = lx-1.0,rvy = 0.0}
											  , {rvx = ~dx,   rvy = ~dy}
											  ]
							  , verDigitBar	= [ {rvx = dx,    rvy = dy}
							  				  , {rvx = 0.0,   rvy = 0.5-ly}
							  				  , {rvx = ~dx,   rvy = dy}
							  				  , {rvx = ~dx,   rvy = ~dy}
							  				  , {rvx = 0.0,   rvy = ly-0.5}
							  				  , {rvx = dx,    rvy = ~dy}
							  				  ]
							  }
		(hmargin,vmargin)	= (0.12,0.12)
		(dx,dy)				= (0.06,0.06)
		(lx,ly)				= (2.0*(hmargin+dx),vmargin+dy)
		digitshapes         :: {!DigitShapeTT}
		digitshapes			= {shape0,shape1,shape2,shape3,shape4,shape5,shape6,shape7,shape8,shape9}
		shape0				= { horDigitBars = [a,c],   verDigitBars = [a,b,d,e] }
		shape1				= { horDigitBars = [],      verDigitBars = [d,e]     }
		shape2				= { horDigitBars = [a,b,c], verDigitBars = [b,d]     }
		shape3				= { horDigitBars = [a,b,c], verDigitBars = [d,e]     }
		shape4				= { horDigitBars = [b],     verDigitBars = [a,d,e]   }
		shape5				= { horDigitBars = [a,b,c], verDigitBars = [a,e]     }
		shape6				= { horDigitBars = [a,b,c], verDigitBars = [a,b,e]   }
		shape7				= { horDigitBars = [a],     verDigitBars = [d,e]     }
		shape8				= { horDigitBars = [a,b,c], verDigitBars = [a,b,d,e] }
		shape9				= { horDigitBars = [a,b,c], verDigitBars = [a,d,e]   }
		[a,b,c,d,e:_]		= [ {rx=hmargin,    ry=vmargin}
							  , {rx=hmargin,    ry=0.5}
							  , {rx=hmargin,    ry=1.0-vmargin}
							  , {rx=1.0-hmargin,ry=vmargin}
							  , {rx=1.0-hmargin,ry=0.5}
							  ]
		
		receiverfun :: !DigitDisplayMsgIn !*(DigitDisplaySt,PSt .ps) -> (DigitDisplayMsgOut,(DigitDisplaySt,PSt .ps))
		receiverfun (SetDigitValueIn newValue) (ddst,pst=:{io})
			# ddst	= {ddst & value = okValue}
			# io	= setControlLook customId True (True,displaylook (format okDigitFormat okValue)) io
			= (SetDigitValueOut,(ddst,{pst & io=io}))
		where
			okValue	= validateDigitValue okDigitFormat newValue
		receiverfun GetDigitValueIn (ddst=:{value},pst)
			= (GetDigitValueOut value,(ddst,pst))
		
		displaylook :: ![Digit] !SelectState !UpdateState !*Picture -> *Picture
		displaylook digits _ _ picture
			# picture	= unfill {zero & corner2={x=customControlSize.w,y=customControlSize.h}} picture
			# picture	= sseq [fillPolygons {x=i*okDigitSize.w,y=0} digitshapes.[digit] \\ digit <- digits & i <- [0..]] picture
			= picture
		where
			fillPolygons base {horDigitBars,verDigitBars} picture
				= sseq
					( [fillAt (base + scaleRPoint2 okDigitSize posTT) {polygon_shape=map (scaleRVector2 okDigitSize) digitbars.horDigitBar} \\ posTT <- horDigitBars]
						++
					  [fillAt (base + scaleRPoint2 okDigitSize posTT) {polygon_shape=map (scaleRVector2 okDigitSize) digitbars.verDigitBar} \\ posTT <- verDigitBars]
					) picture
		
	getControlType _ = "DigitDisplay"

getDigitDisplayValue :: !DigitDisplayId !(PSt .ps) -> (!Int,!PSt .ps)
getDigitDisplayValue {receiverId} pst
	= case syncSend2 receiverId GetDigitValueIn pst of
		((SendOk,Just (GetDigitValueOut x)),pst) -> (x,pst)
		other                                    -> sendError "getDigitDisplayValue"

setDigitDisplayValue :: !DigitDisplayId !Int !(PSt .ps) -> PSt .ps
setDigitDisplayValue {receiverId} newValue pst
	= case syncSend2 receiverId (SetDigitValueIn newValue) pst of
		((SendOk,Just SetDigitValueOut),pst) -> pst
		other                                -> sendError "setDigitDisplayValue"

sendError fName
	= abort (fName +++ ": wrong reply from DigitDisplay.\n")


//	Validate format:
validateDigitFormat :: !DigitFormat -> DigitFormat
validateDigitFormat (IntegerFormat nrDigits)
	= IntegerFormat (setbetween nrDigits 1 maxNrDigits)
where
	maxInt		= 0x7FFFFFFF
	maxNrDigits	= toInt (log10 (toReal maxInt))-1


//	getNrOfDigits returns the number of digits to display:
getNrOfDigits :: !DigitFormat -> Int
getNrOfDigits (IntegerFormat nrDigits) = nrDigits

//	Validate digit size:
validateDigitSize :: !DigitSize -> Size
validateDigitSize {w,h}
	= {w=max 1 w,h=max 1 h}

//	Validate value according to valid format:
validateDigitValue :: !DigitFormat !Int -> Int
validateDigitValue (IntegerFormat nrDigits) x
	= setbetween x 0 (10^nrDigits-1)

//	Format integer values:
::	Digit	:== Int		// A digit is an int in [0..9]

class format a :: !DigitFormat !a -> [Digit]

instance format Int where
	format (IntegerFormat nrDigits) x
		= getdigits nrDigits (abs x) []
	where
		getdigits :: !Int !Int ![Digit] -> [Digit]
		getdigits 0 _ digits
			= digits
		getdigits n x digits
			= getdigits (n-1) (x / 10) [x rem 10 : digits]

digitsToString :: ![Digit] -> String
digitsToString digits
	= {toChar (toInt '0'+d) \\ d<-digits}


instance toString DigitFormat where
	toString (IntegerFormat nrDigits) = "(IntegerFormat " <+++ nrDigits <+++")"


scaleRVector2 :: !Size !RVector2 -> Vector2
scaleRVector2 {w,h} {rvx,rvy} = {vx=toInt ((toReal w)*rvx), vy=toInt ((toReal h)*rvy)}

scaleRPoint2 :: !Size !RPoint2 -> Point2
scaleRPoint2 {w,h} {rx,ry} = {x=toInt ((toReal w)*rx), y=toInt ((toReal h)*ry)}
