implementation module ListOverloading

import StdEnv
import StdDebug
import ListGenerator

instance zero [a] | zero a where zero = trace_n "zero not yet implemented" []

instance one  [a] | one  a where one = trace_n "one not yet implemented" []

instance ~    [a] | ~    a where ~ _ = trace_n "~ not yet implemented" []

instance +    [a] | +    a where + _ _ = trace_n "+ not yet implemented" []

instance -    [a] | -    a where - _ _ = trace_n "- not yet implemented" []

instance *    [a] | *    a where * _ _ = trace_n "* not yet implemented" []

instance /    [a] | /    a where / _ _ = trace_n "/ not yet implemented" []

Start  = (test [1,2,3], test [1.0,2.0,3.0])

test a = ( zero + a == a && a == a + zero
         , a - zero == a && a == ~ (zero - a)
         ,  one * a == a && a == a * one
         ,  a / one == a
         ,  ~ (~ a) == a
         )
