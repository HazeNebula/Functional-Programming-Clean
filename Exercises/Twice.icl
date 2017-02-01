module Twice

//	Set the Stack Size to 1M.

import StdEnv

Start = ('\n',2^0,                          inc 0
        ,'\n',2^1,                    twice inc 0
        ,'\n',2^2,              twice twice inc 0
        ,'\n',2^4,        twice twice twice inc 0
        ,'\n',2^16, twice twice twice twice inc 0
        )

