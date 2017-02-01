implementation module StdNum

import StdEnv
import StdDebug
//	import StdQ only if you implemented it
//import StdQ

::  Num	= Int  Int 
        | Real Real 
//	add this alternative only if you implemented StdQ
//        | Q    Q

instance ==     Num where == _ _ = trace_n "instance == Num not yet implemented" False

instance <      Num where < _ _ = trace_n "instance < Num not yet implemented" False

instance +      Num where + _ _ = trace_n "instance + Num not yet implemented" zero

instance -      Num where - _ _ = trace_n "instance - Num not yet implemented" zero

instance zero   Num where zero = trace_n "instance zero Num not yet implemented" (abort "instance zero not yet implemented")

instance *      Num where * _ _ = trace_n "instance * Num not yet implemented" zero

instance /      Num where / _ _ = trace_n "instance / Num not yet implemented" zero

instance one    Num where one = trace_n "instance one Num not yet implemented" zero

instance abs    Num where abs _ = trace_n "instance abs Num not yet implemented" zero

	
instance sign   Num where sign _ = trace_n "instance sign Num not yet implemented" zero

	
instance ~      Num where ~_ = trace_n "instance ~ Num not yet implemented" zero

instance toInt  Num where toInt _ = trace_n "instance toInt Num not yet implemented" zero

	
instance toReal Num where toReal _ = trace_n "instance toReal Num not yet implemented" zero

	
// implement this instance only if you have implemented StdQ:
//instance toQ    Num where toQ _ = trace_n "instance toQ Num not yet implemented" zero

class    fromNum a :: !Num -> a
instance fromNum Int  where fromNum _ = trace_n "instance fromNum Int not yet implemented" zero

instance fromNum Real where fromNum _ = trace_n "instance fromNum Real not yet implemented" zero

	
// implement this instance only if you have implemented StdQ:
//instance fromNum Q where fromNum _ = trace_n "instance fromNum Q not yet implemented" zero

class toNum a :: !a -> Num
instance toNum Int where toNum _ = trace_n "instance toNum Int not yet implemented" zero

instance toNum Real where toNum _ = trace_n "instance toNum Real not yet implemented" zero

// implement this instance only if you have implemented StdQ:
//instance toNum Q where toNum _ = trace_n "instance toNum Q not yet implemented" zero

instance toString Num where toString _ = trace_n "instance toString Num not yet implemented" ""

