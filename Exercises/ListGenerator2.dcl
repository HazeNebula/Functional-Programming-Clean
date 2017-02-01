definition module ListGenerator2

import StdClass

all_elements	::     a -> [a]
step			::     a -> [a] | IncDec a & Ord a
from_step		::   a a -> [a] | Enum a
from_to			::   a a -> [a] | Enum a
from_to_step	:: a a a -> [a] | Enum, == a
