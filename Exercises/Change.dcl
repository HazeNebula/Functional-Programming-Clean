definition module Change

::  Amount     :== Int         // a positive number
::  Currency   :== Int         // a positive number
::  Currencies :== [Currency]  // a non-empty list
::  Coin       :== Int         // a positive number
::  K          :== Int         // a positive number
::  Change     :== [Coin]

//	Implement and test the following function:
change :: Amount Currencies K -> [Change]
