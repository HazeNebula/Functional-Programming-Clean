implementation module StdT

import StdEnv
import StdDebug

::	T  // define your implementation of T here: 

instance ==			T where == _ _ = trace_n "instance == T not yet implemented" False

instance <			T where < _ _ = trace_n "instance < T not yet implemented" False

instance zero		T where zero = abort "instance zero T not yet implemented"

instance +			T where + _ _ = trace_n "instance + T not yet implemented" zero

instance -			T where - _ _ = trace_n "instance - T not yet implemented" zero

instance toInt		T where toInt _ = trace_n "instance toInt T not yet implemented" zero

instance fromInt	T where fromInt _ = trace_n "instance fromInt T not yet implemented" zero

instance toString	T where toString _ = trace_n "instance toString T not yet implemented" ""

instance fromString	T where fromString _ = trace_n "instance fromString T not yet implemented" zero

