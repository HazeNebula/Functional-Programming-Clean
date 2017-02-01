definition module StdStateMonad

import StdMonad

:: ST s a

instance return (ST s)
instance >>=    (ST s)

mkST :: (s -> (a,s)) -> ST s a
unST :: (ST s a) -> s -> (a, s)
