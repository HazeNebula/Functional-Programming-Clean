implementation module ListGenerator

import StdEnv
import StdDebug

all_elements			:: a -> [a]
all_elements _ = trace_n "all_elements not yet implemented" []

step					:: a -> [a] | +, one a
step _ = trace_n "step not yet implemented" []

from_step				:: a a -> [a] | + a
from_step _ _ = trace_n "from_step not yet implemented" []

from_to					:: a a -> [a] | Ord, PlusMin, one a
from_to _ _ = trace_n "from_to not yet implemented" []

from_to_step			:: a a a -> [a] | Ord, PlusMin a
from_to_step _ _ _ = trace_n "from_to_step not yet implemented" []

Start					= ( take 10 (all_elements 'H')
						  , '\n'
						  , take 26 (step 'a')
						  , '\n'
						  , take 10 (from_step 0 -2)
						  , '\n'
						  , from_to 0 10
						  , '\n'
						  , from_to_step 0 -10 2
						  )
