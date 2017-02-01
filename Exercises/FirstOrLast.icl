implementation module FirstOrLast

import StdEnv
import StdDebug

//	1.
first2				:: [a] -> [a]
first2 _ = trace_n "first2 not yet implemented" []

last2				:: [a] -> [a]
last2 _ = trace_n "last2 not yet implemented" []

Start				= (first2 ['a'..'z'], last2 ['a'..'z'])

//	2.
/*
Start = hd (hd (hd [[[1,2,3],[4]],[[5],[6]]]))

Start = hd (tl [1,2,3,4,5])

Start = first2 [[1],[],[2,3],[4,5,6]]

Start = last2 [[1],[],[2,3],[4,5,6]]

*/
/*	You can check the outcome with the following Start line:
Start = (	hd (hd (hd [[[1,2,3],[4]],[[5],[6]]]))
        ,	hd (tl [1,2,3,4,5])
        ,	first2 [[1],[],[2,3],[4,5,6]]
        ,	last2 [[1],[],[2,3],[4,5,6]]
        )
*/

//	3.
firstn				:: Int [a] -> [a]
firstn _ _ = trace_n "firstn not yet implemented" []

lastn				:: Int [a] -> [a]
lastn _ _ = trace_n "lastn not yet implemented" []

//	4.
/*
For all      0 <= n, xs :: [a] : firstn n (firstn n xs) = ...
For all      0 <= n, xs :: [a] : firstn n (lastn  n xs) = ...
For all      0 <= n, xs :: [a] : lastn  n (firstn n xs) = ...
For all      0 <= n, xs :: [a] : lastn  n (lastn  n xs) = ...
For all 0 <= m <= n, xs :: [a] : firstn m (firstn n xs) = ...
For all 0 <= m <= n, xs :: [a] : length   (firstn m xs) ? length (firstn n xs)
*/

