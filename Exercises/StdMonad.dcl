definition module StdMonad

class return        c :: a -> c a
class (>>=) infix 0	c :: (c a) (a -> c b) -> c b
class fail          c :: c a

class Monad	        c | return, >>= c
class MonadFail	    c | Monad, fail c
