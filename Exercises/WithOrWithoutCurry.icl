module WithOrWithoutCurry

import StdEnv
import StdDebug

e1 = curry fst

e2 = curry snd

e3 = uncurry (+)

e4 = uncurry (-)

e5 = uncurry (*)

e6 = uncurry (/)

Start = 42
