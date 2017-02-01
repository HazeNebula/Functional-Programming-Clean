module Flatten

import StdEnv
import StdDebug

/*	1.
flatten [ [x0,x1,x2], [x3,x4], [x5], [] ]

flatten [ [ [x0,x1,x2], [x3,x4] ], [], [ [x5], [] ] ]

flatten (flatten [ [ [x0,x1,x2], [x3,x4] ], [], [ [x5],[] ] ] )

*/

//	With the following definitions, you can test your computations:
x0 = 0
x1 = 1
x2 = 2
x3 = 3
x4 = 4
x5 = 5

Start   = ( flatten [ [x0,x1,x2], [x3,x4], [x5], [] ]
          , flatten [ [ [x0,x1,x2], [x3,x4] ], [], [ [x5], [] ] ]
          , flatten [ [ [0,1,2], [3,4] ], [], [ [5], [] ] ]
          )
