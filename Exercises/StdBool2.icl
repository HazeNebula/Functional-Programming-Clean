implementation module StdBool2

import StdEnv
import StdDebug

lift0 f a				= f a

lift1 f g1 a			= f (g1 a)

lift2 f g1 g2 a			= f (g1 a) (g2 a)

lift3 f g1 g2 g3 a		= f (g1 a) (g2 a) (g3 a)

instance ~~  Bool
where ~~ _ = trace_n "instance ~~ Bool not yet implemented" False

instance ||| Bool
where ||| _ _ = trace_n "instance ||| Bool not yet implemented" False

instance &&& Bool
where &&& _ _ = trace_n "instance &&& not yet implemented" False

instance ~~  (a -> Bool)
where ~~ _ = trace_n "instance ~~ (a -> Bool) not yet implemented" (const False)

instance ||| (a -> Bool)
where ||| _ _ = trace_n "instance ||| (a -> Bool) not yet implemented" (const False)

instance &&& (a -> Bool)
where &&& _ _ = trace_n "instance &&& (a -> Bool) not yet implemented" (const False)

Start = ( filter ((<) 3 &&& (>) 8) [1..10]
        , filter (~~ ((==) 5))     [1..10]
        )
