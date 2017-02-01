implementation module StdStateMonad

import StdMonad

:: ST s a = ST (s -> (a, s))

instance return (ST s) where return x     = ST (\w = (x, w))
instance >>=    (ST s) where >>= (ST f) g = ST (\w = let (a, w1) = f w 
                                                      in unST (g a) w1
                                               )

mkST :: (s -> (a,s)) -> ST s a
mkST f = ST f

unST :: (ST s a) -> s -> (a, s)
unST (ST f) = f
