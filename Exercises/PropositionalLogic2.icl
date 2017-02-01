implementation module PropositionalLogic2

import StdEnv
import StdDebug

Start = 42

//	Representation of propositional logic terms:
::	PropL //define your implementation of PropL

::	Ident // define your implementation of variable identifiers

//	Displaying PropL terms:
instance toString PropL where
	toString _ = trace_n "instance toString PropL not yet implemented" ""

//	Evaluation of ground terms:
eval1						:: PropL -> Bool
eval1 _ = trace_n "eval1 not yet implemented" False

//	Evaluation of terms with variables:
eval2						:: Valuation PropL -> Bool
eval2 _ _ = trace_n "eval2 not yet implemented" False

//	Filtering variables:
vars						:: PropL -> [Ident]
vars _ = trace_n "vars not yet implemented" []

//	All possible valuations for variables:
::	Valuation // give a definition for Valuation here

vals						:: [Ident] -> [Valuation]
vals _ = trace_n "vals not yet implemented" []

//	When is it true?
truths						:: PropL -> [Valuation]
truths _ = trace_n "truths not yet implemented" []

