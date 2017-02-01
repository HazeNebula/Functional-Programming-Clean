module FindTheRedex

import StdEnv
import StdDebug

e1    = 42

e2    = 1 + 125 * 8 / 10 - 59
e2    = 1 + 1000 / 10 - 59
e2    = 1 + 100 - 59
e2    = 101 - 59
e2    = 42

e3    = not True || True && False
e3    = False || True && False
e3    = False || False
e3    = False

e4    =  1 + 2 == 6 - 3
e4    = 3 == 6 - 3
e4    = 3 == 3
e4    = True

e5    = "1 + 2" == "6 - 3"
e5    = False


e6    = "1111 + 2222" == "1111" +++ " + " +++ "2222"
e6    = "1111 + 2222" == "1111 + " +++ "2222"
e6    = "1111 + 2222" == "1111 + 2222"
e6    = True

Start = e6
