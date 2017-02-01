module NotationADT

import StdTuple, StdInt
import StdDebug

:: Day       = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
:: Nat       = Nat Int
:: Number    = Whole Nat | Decimal Real
:: Functions = F0 Int | F1 (Int -> Int) | F2 (Int Int -> Int)
:: Void      = Void

Start = 42
