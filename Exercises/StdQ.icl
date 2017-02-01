implementation module StdQ

import StdEnv
import StdDebug

::	Q  // define your implementation of Q here

instance zero   Q where zero = abort "instance zero Q not yet implemented"

instance one    Q where one = trace_n "instance one Q not yet implemented" zero

instance ==     Q where == _ _ = trace_n "instance == Q not yet implemented" False

instance <      Q where < _ _ = trace_n "instance < Q not yet implemented" False

instance +      Q where + _ _ = trace_n "instance + Q not yet implemented" zero

instance -      Q where - _ _ = trace_n "instance - Q not yet implemented" zero

instance *      Q where  * _ _ = trace_n "instance * Q not yet implemented" zero

instance /      Q where / _ _ = trace_n "instance / Q not yet implemented" zero

instance abs    Q where abs _ = trace_n "instance abs Q not yet implemented" zero

instance sign   Q where sign _ = trace_n "instance sign Q not yet implemented" 0

instance ~      Q where ~ _ = trace_n "instance ~ Q not yet implemented" zero 

instance toInt  Q where toInt _ = trace_n "instance toInt Q not yet implemented" zero

instance toReal Q where toReal _ = trace_n "instance toReal Q not yet implemented" zero

isInt									:: Q -> Bool
isInt _ = trace_n "isInt not yet implemented" False

class toQ a :: a -> Q
instance toQ Int  where toQ _ = trace_n "instance toQ Int not yet implemented" zero

instance toQ Real where	toQ _ = trace_n "instance toQ Real not yet implemented" zero

instance toQ (Int,Int) where toQ _ = trace_n "instance toQ (Int,Int) not yet implemented" zero

instance toQ (Int,Int,Int) where toQ _ = trace_n "instance toQ (Int,Int,Int) not yet implemented" zero

instance toString Q where toString _ = trace_n "instance toString Q not yet implemented" ""

