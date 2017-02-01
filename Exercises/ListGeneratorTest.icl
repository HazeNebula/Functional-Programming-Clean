module ListGeneratorTest

/*	Test module ListGenerator and ListGenerator2
	for working with Gast: 
		(*) use Environment 'Gast'
		(*) set Project Options to 'Basic Values Only' en '2M' Maximum Heap Size
*/

import gast
import ListGenerator
//import ListGenerator2

Start
												= testn 1000
													(\x y z ->
													      firstn_equal                       x
													   /\ from_incremented_with_one          x
													   /\ from_step_incremented_with_step    x y
													   /\ from_to_incremented_with_one       x y
													   /\ from_to_step_increments_with_step  x y z
													   /\ from_to_step_0_is_empty            x y
													   /\ True
													)

max_list_length									= 1000

firstn_equal									:: Int -> Property
firstn_equal x									= name "first n in list are equal"
											       (let l` = take max_list_length (all_elements x) 
											            n  = length l`
											         in n == max_list_length && all ((==) x) l`
											       )

from_incremented_with_one							:: Int -> Property
from_incremented_with_one x						= name "step increments with one"
										           (let l` = take max_list_length (step x) 
											            n  = length l`
										             in n == max_list_length && all ((==) one) (difference l`)
										           )

from_step_incremented_with_step					:: Int Int -> Property
from_step_incremented_with_step x y				= name "from_step increments with step"
										           (let l` = take max_list_length (from_step x y) 
											            n  = length l`
										             in n == max_list_length && all ((==) y) (difference l`)
										           )

from_to_incremented_with_one					:: Int Int -> Property
from_to_incremented_with_one x y				= name "from_to increments with one"
										           (let l` = take max_list_length (from_to x y)
										                n  = length l`
										             in if (x > y) (n == 0) (all ((==) one) (difference l`))
										           )

from_to_step_increments_with_step				:: Int Int Int -> Property
from_to_step_increments_with_step x y z			= name "from_to_step increments with step"
										           (ok ==> let l` = take max_list_length (from_to_step x y z)
										                       n  = length l`
										                    in if (x <= y && z > 0) (all ((==) z) (difference l`))
										                      (if (x >= y && z < 0) (all ((==) z) (difference l`))
										                                            (n == 0)
										                      )
										           )
	where ok = z <> 0
	             && 
	           all (\x->x <> abs x || x==0) [abs x, abs y, abs z]

from_to_step_0_is_empty							:: Int Int -> Property
from_to_step_0_is_empty x y 					= name "from_to_step with step 0 is empty"
										           (isEmpty (from_to_step x y 0))

difference										:: [a] -> [a] | - a
difference [a,b:as]								= [b-a : difference [b:as]]
difference _									= []
