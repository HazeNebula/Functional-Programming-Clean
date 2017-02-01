implementation module Change

import StdEnv
import StdDebug

::  Amount     :== Int         // a positive number
::  Currency   :== Int         // a positive number
::  Currencies :== [Currency]  // a non-empty list
::  Coin       :== Int         // a positive number
::  K          :== Int         // a positive number
::  Change     :== [Coin]

//	Implement and test the following function:
change :: Amount Currencies K -> [Change]
change _ _ _ = trace_n "change not yet implemented" []

//	Test-cases from workout:
Start = ( change 50 [100,50,20,10,5,1] 1, '\n'
        , change 50 [100,50,20,10,5,1] 2, '\n'
        , change 50 [100,50,20,10,5,1] 3, '\n'
        , change 50 [100,50,20,10,5,1] 4
        )
