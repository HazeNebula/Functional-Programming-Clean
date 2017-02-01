implementation module StdListMonad

import StdMonad

instance return	[] where return x = [x]
instance >>=	[] where >>= xs f = [y \\ x <- xs, y <- f x]
instance fail   [] where fail     = []
