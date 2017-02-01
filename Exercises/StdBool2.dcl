definition module StdBool2

import StdBool

class    ~~            a ::    !a -> a
class   (|||) infixr 2 a :: !a !a -> a
class   (&&&) infixr 3 a :: !a !a -> a

instance ~~  Bool
instance ||| Bool
instance &&& Bool

instance ~~  (a -> Bool)
instance ||| (a -> Bool)
instance &&& (a -> Bool)
