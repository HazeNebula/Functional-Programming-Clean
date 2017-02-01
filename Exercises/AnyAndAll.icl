module AnyAndAll

import StdEnv
import StdDebug

and` = abort "and` not yet implemented"

or` = abort "or` not yet implemented"

all_l _ _ = trace_n "all_l not yet implemented" False

all_r _ _ = trace_n "all_r not yet implemented" False

any_l _ _ = trace_n "any_l not yet implemented" False

any_r _ _ = trace_n "any_r not yet implemented" False

Start = all_l id [False:repeat True ]
//Start = any_l id [True :repeat False]
//Start = all_r id [False:repeat True ]
//Start = any_r id [True :repeat False]
