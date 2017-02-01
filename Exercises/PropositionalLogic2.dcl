definition module PropositionalLogic2

import StdEnv

//	Definition PropL should be copied from PropositionalLogic2.icl
::	PropL

//	Definition Ident should be copied from PropositionalLogic2.icl
::	Ident

//	Definition Valuation should be copied from PropositionalLogic2.icl
::	Valuation

instance toString PropL

eval1						::           PropL -> Bool
eval2						:: Valuation PropL -> Bool
vars						::           PropL -> [Ident]
vals						:: [Ident]         -> [Valuation]
truths						::           PropL -> [Valuation]
