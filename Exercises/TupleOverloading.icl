implementation module TupleOverloading

import StdEnv
import StdDebug

instance +    (a,b)   | + a & + b                where + _ _ = abort "instance + (a,b) not yet implemented"

instance +    (a,b,c) | + a & + b & + c          where + _ _ = abort "instance + (a,b,c) not yet implemented"

instance -    (a,b)   | - a & - b                where - _ _ = abort "instance - (a,b) not yet implemented"

instance -    (a,b,c) | - a & - b & - c          where - _ _ = abort "instance - (a,b,c) not yet implemented"

instance *    (a,b)   | * a & * b                where * _ _ = abort "instance * (a,b) not yet implemented"

instance *    (a,b,c) | * a & * b & * c          where * _ _ = abort "instance * (a,b,c) not yet implemented" 

instance /    (a,b)   | / a & / b                where / _ _ = abort "instance / (a,b) not yet implemented"

instance /    (a,b,c) | / a & / b & / c          where / _ _ = abort "instance / (a,b,c) not yet implemented"

instance zero (a,b)   | zero a & zero b          where zero = abort "instance zero (a,b) not yet implemented"

instance zero (a,b,c) | zero a & zero b & zero c where zero = abort "instance zero (a,b,c) not yet implemented"

instance one  (a,b)   | one a & one b            where one = abort "instance one (a,b) not yet implemented"

instance one  (a,b,c) | one a & one b & one c    where one = abort "instance one (a,b,c) not yet implemented"

instance ~    (a,b)   | ~ a & ~ b                where ~ _ = abort "instance ~ (a,b) not yet implemented"

instance ~    (a,b,c) | ~ a & ~ b & ~ c          where ~ _ = abort "instance ~ (a,b,c) not yet implemented"

//	You can use this function to test your implementation:
Start  = (test (1,2), test (1,2,3))

test a = ( zero + a == a    && a    == a + zero
         , a - zero == a    && a    == ~ (zero - a)
         ,  one * a == a    && a    == a * one
         , zero * a == zero && zero == a * zero
         ,  a / one == a
         ,  ~ (~ a) == a
         )
