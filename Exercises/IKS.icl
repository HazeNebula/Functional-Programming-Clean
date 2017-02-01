module IKS

/** To work with dynamics:
	(1) create a new project
	(2) set the environment to 'Everything'
	(3) in 'Project:Project Options...' check 'Enable dynamics'
*/
import StdEnv
import StdDebug
import StdMaybe
import StdDynamic, StdDynamicFileIO
import StringUtil

/**	An interpreter for IKS.
*/

i			:: a -> a
i x     	= x

k			:: a b -> a
k x y		= x

s			:: .(a -> .(.b -> .c)) .(a -> .b) a -> .c
s x y z 	= x z (y z)

//	2. Parsing IKS expressions
::	IKS										= I | K | S | N Int | App IKS IKS

//	3. Interpreting IKS expressions

interp										:: (Dynamic,Dynamic,Dynamic) IKS -> Dynamic
interp _ _ = trace_n "interp not yet implemented" (dynamic "interp not yet implemented" :: String)

dynApply									:: Dynamic Dynamic -> Dynamic
dynApply (f :: a -> b) (x :: a)				= dynamic f x :: b
dynApply     _             _				= dynamic "dynamic type error"

//	4. Console

Start										:: *World -> *World
Start world = trace_n "Start not yet implemented" world

