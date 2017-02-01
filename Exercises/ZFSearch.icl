implementation module ZFSearch

import StdEnv
import StdDebug

(??) infixl 9 :: ![a] !a -> Int | Eq a
(??) _ _ = trace_n "(??) not yet implemented" zero

//	You can use this Start-function to test your implementation. The result should be True.
Start = [ [1,2,3,4,5,6]   ?? 3
        , [1,2,3,4,5,6]   ?? 10
        , ['Hello world'] ?? 'o'
        ]
        ==
        [ 2, -1, 4 ]
