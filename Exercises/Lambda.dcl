definition module Lambda

import StdEnv

:: Term			    = C Value					// constant  v             (C v)
                    | X Index					// variable  x_i           (X i)
				    | (@.) infixl 7 Term  Term	// application (t1 t2)     (t1 @. t2)
				    |  \.           Index Term	// abstraction (\x_i . t)  (\. i t)
:: Value		  :== Int						// arbitrary integer value
:: Index		  :== Int						// index (customarily       i >= 0)

instance toString Term

nf					:: Term              -> Bool
vars				:: Term              -> [Index]
(<:) infixl 6		:: Term (Index,Term) -> Term

normal_order		::                Term -> Term
applicative_order	::                Term -> Term
rewrite 			:: (Term -> Term) Term -> Term
