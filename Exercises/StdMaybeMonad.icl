implementation module StdMaybeMonad

import StdMonad

:: Maybe a = Nothing | Just a

instance return	Maybe where return x       = Just x
instance >>=	Maybe where >>= (Just x) f = f x
                            >>= Nothing  f = Nothing
instance fail   Maybe where fail           = Nothing
