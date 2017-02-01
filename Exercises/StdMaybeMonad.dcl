definition module StdMaybeMonad

import StdMonad

:: Maybe a = Nothing | Just a

instance return	Maybe
instance >>=	Maybe
instance fail   Maybe
