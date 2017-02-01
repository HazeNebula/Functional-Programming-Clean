module ReduceLists

import StdEnv
import StdDebug

/*
//	1.
Start
= [] ++ []

//	2.
Start
= [] ++ [x0,x1] ++ []

//	3.
Start
= [[]] ++ [x0,x1]

//	4.
Start
= [[x0,x1]] ++ [[]]

// 5.
Start
= [x0,x1,x2] ++ ([x3,x4] ++ ([x5] ++ []))

// 6.
Start
= (([x0,x1,x2] ++ [x3,x4]) ++ [x5]) ++ []

*/

//	You can use the following definitions and Start-rule to check your derivations:
x0 = 0; x0` = [x0]
x1 = 1; x1` = [x1]
x2 = 2
x3 = 3
x4 = 4
x5 = 5

Start = ( [] ++ []
        , [] ++ [x0,x1] ++ []
        , [[]] ++ [x0`,x1`]
        , [[x0,x1]] ++ [[]]
        , [x0,x1,x2] ++ ([x3,x4] ++ ([x5] ++ []))
        , (([x0,x1,x2] ++ [x3,x4]) ++ [x5]) ++ []
        )
