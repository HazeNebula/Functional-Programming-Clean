implementation module FarmerWantsAWife

import StdEnv
import StdDebug

/*
 * If N is an even number, then there exists an algorithm that computes a stable coupling.
 * This algorithm was proven correct in 1962 by David Gale and Lloyd Shaley.
 * It is an iterative algorithm that keeps computing the following:
 * find a not yet matched suitor and find the most favourite partner that he hasn't proposed to yet.
 * The suitor is rejected if the partner is already coupled with a suitor that they prefer.
 * In any other case the partner accepts; if they were coupled with another suitor, that person is now single.
 * This repeats until all suitors are coupled.
 */

::	Nr						:== Int								// 1..N

suitors						= [[1 .. 4]
                              ,[1 .. 4]
                              ,[1 .. 4]
                              ,[1 .. 4]
                              ]
partners					= map reverse
							  [[1 .. 4]
                              ,[1 .. 4]
                              ,[1 .. 4]
                              ,[1 .. 4]
                              ]

Start						= farmer_wants_a_wife (suitors,partners)

farmer_wants_a_wife			:: ([[Nr]],[[Nr]]) -> [(Nr,Nr)]
farmer_wants_a_wife _ = trace_n "farmer_wants_a_wife not yet implemented" []

/*	Correctness test for input; not really a part of the algorithm.
	You can use it before running farmer_wants_a_wife.
*/
preferences_ok				:: [[Nr]] -> (Bool,String)
preferences_ok as
| isOdd nr_as				= (False, "Number of persons is odd: " <+ nr_as <+ ".")
| not (preferences_ok as)	= (False, "Population does not have all preferences complete.")
| otherwise					= (True,  "")
where
	nr_as					= length as
	all_nrs					= [1..nr_as]
	preferences_ok			= all (\preferences -> sort preferences == all_nrs)

input_ok					:: ([[Nr]],[[Nr]]) -> (Bool,String)
input_ok (suitors,partners)
| suitors_ok && partners_ok	= if (length suitors <> length partners) (False, "Unequal number of suiters vs partners.") (True,"")
| otherwise					= (False, s_msg <+ "\n" <+ p_msg <+ "\n")
where
	(suitors_ok, s_msg)		= preferences_ok suitors
	(partners_ok,p_msg)		= preferences_ok partners

(<+) infixl					:: !String !a -> String | toString a
(<+) str a					= str +++ toString a
