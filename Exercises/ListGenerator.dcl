definition module ListGenerator

import StdClass

all_elements	:: a     -> [a]
step			:: a     -> [a] | +, one a
from_step		:: a a   -> [a] | + a
from_to			:: a a   -> [a] | Ord, PlusMin, one a
from_to_step	:: a a a -> [a] | Ord, PlusMin a
