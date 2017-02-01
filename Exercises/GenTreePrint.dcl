definition module GenTreePrint

import StdClass
import GenTree

instance toString (GenTree a b) | toString a & toString b
