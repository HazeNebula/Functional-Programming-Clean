implementation module RefactorXX

import StdClass, StdInt, StdList, StdOverloaded, StdString
import StdDebug
import StringUtil

Start :: [Val Int]
Start = map eval [E1,E2,E3,E4,E5]

/*	These expressions are included for easy reference: they are the same as in the text.
	One extra expression has been added (E6). 
	Note that some expressions are syntactically incorrect Clean expressions!

E1 = (let x = 42 - 3 in x / 0) + (let y = 6 in y * y)
E2 = let x = 42 in x + (let x = 58 in x)
E3 = let x = 1 in let y = 2 in let x = 3 in 4
E4 = let x = 1 in x + y
E5 = (let x = 1 in x) * x
E6 = let x = 5 in let y = x in 0
*/

E1 = (OP (LET "x" (OP (NR 42) MIN (NR 3)) (OP (VAR "x") DIV (NR 0))) PLUS (LET "y" (NR 6) (OP (VAR "y") MUL (VAR "y"))))
E2 = (LET "x" (NR 42) (OP (VAR "x") PLUS (LET "x" (NR 58) (VAR "x"))))
E3 = (LET "x" (NR 1) (LET "y" (NR 2) (LET "x" (NR 3) (NR 4))))
E4 = (LET "x" (NR 1) (OP (VAR "x") PLUS (VAR "y")))
E5 = (OP (LET "x" (NR 1) (VAR "x")) MUL (VAR "x"))
E6 = (LET "x" (NR 5) (LET "y" (VAR "x") (NR 0)))

::	Expr							= NR   Int
									| VAR  Name
									| OP   Expr Operator Expr
									| LET  Name     Expr Expr
::	Name							:== String
::	Operator						= PLUS | MIN | MUL | DIV
::	Val a							= Result a | Undef

class fail          c :: c a
class return        c :: a -> c a
class (>>=) infix 0 c :: (c a) (a -> c b) -> c b
class Monad         c | return, >>= c
class MonadFail     c | Monad, fail c

instance fail   Val where fail              = Undef
instance return Val where return a          = Result a
instance >>=    Val where >>= (Result a) fb = fb a
                          >>= undef      _  = Undef

instance fail   []  where fail				= []
instance return []  where return x			= [x]
instance >>=    []  where >>= xs f			= flatten (map f xs)

//  printing expressions:
instance toString Expr where
	toString _ = trace_n "instance toString Expr not yet implemented" ""

//	free variables:
free								:: Expr -> [Name]
free _ = trace_n "free not yet implemented" []

//	remove subexpressions with unused let variables:
remove_unused_lets					:: Expr -> Expr
remove_unused_lets expr = trace_n "remove_unused_lets not yet implemented" expr

eval								:: Expr -> c Int | MonadFail c
eval _ = trace_n "eval not yet implemented" fail

