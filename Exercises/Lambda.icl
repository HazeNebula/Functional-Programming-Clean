implementation module Lambda

import StdEnv
import StdDebug

:: Term		                    = C Value					                        // constant  v             (C v)
                                | X Index					                        // variable  x_i           (X i)
                                | (@.) infixl 7 Term  Term	                        // application (t1 t2)     (t1 @. t2)
                                |  \.           Index Term	                        // abstraction (\x_i . t)  (\. i t)
:: Value	                    :== Int						                        // arbitrary integer value
:: Index	                    :== Int						                        // index (customarily       i >= 0)

t0								= (C 42)											// 42
t1								= (X 0)												// x_0
t2								= (\.0 (X 0))										// (\x_0 . x_0)
t3								= (\.0 (X 0)) @. (C 42)								// (\x_0 . x_0) 42
t4								= (\.0 ((X 0) @. (X 0))) @. (\.1 ((X 1) @. (X 1)))	// (\x_0 . x_0 x_0) (\x_1 . x_1 x_1)
t5								= (\.0 (\.1 (X 0)))									// (\x_0 . (\x_1 . x_0))
t6								= (\.0 (\.1 (X 0))) @. (C 42) @. t4					// ((\x_0 . (\x_1 . x_0)) 42) ((\x_0 . x_0 x_0) (\x_1 . x_1 x_1))

terms							= [t0, t1, t2, t3, t4, t5, t6]

//	Convert terms to String:
instance toString Term where
	toString _ = trace_n "instance toString Term not yet implemented" ""

Start = foldr (\s1 s2 -> s1 +++ "\n" +++ s2) "\n" (map toString terms)

//	Determine whether a term is in normal form (does not contain a redex):
nf								:: Term -> Bool
nf _ = trace_n "nf not yet implemented" False

//Start = map nf terms

vars							:: Term -> [Index]
vars _ = trace_n "vars not yet implemented" []

//Start = map vars terms

fresh							:: [Term] -> Index
fresh _ = trace_n "fresh not yet implemented" zero

//	Uniform substitution
(<:) infixl 6					:: Term (Index,Term) -> Term
(<:) term _ = trace_n "(<:) not yet implemented" term

//Start = (((X 2) @. t1) @. (\.0 t1)) <: (0,C 50)

beta_reduce						:: Term Term -> Term
beta_reduce term _ = trace_n "beta_reduce not yet implemented" term

//	Reduction strategies:
normal_order					:: Term -> Term
normal_order term = trace_n "normal_order not yet implemented" term

applicative_order				:: Term -> Term
applicative_order term = trace_n "applicative_order not yet implemented" term

/*
Start = (
		        toString t
		, '\n', toString (normal_order t)
		, '\n', toString (applicative_order t)
		)
where
//	t = (\.0 (\.1 (X 0))) @. ((\.0 (X 0)) @. (C 42)) @. (C 50)
	t = t4
*/

//	Rewrite to normal form:
rewrite						:: (Term -> Term) Term -> Term
rewrite _ term = trace_n "rewrite not yet implemented" term

//Start							= rewrite normal_order t3

