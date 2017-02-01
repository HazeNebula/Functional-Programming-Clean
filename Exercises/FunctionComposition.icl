module FunctionComposition

import StdEnv
import StdDebug

e1 = ((*) 5) o ((+) 1)

e2 = ((+) 1) o ((*) 5)

e3 = ((*) 2) o ((*) 2)

e4 = (min 100) o (max 0)

e5 = ((<) 2) o length

Start = 42
