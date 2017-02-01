definition module StdT

import StdOverloaded

::	T  // T is an abstract data type

instance ==			T
instance <			T

instance zero		T
instance +			T
instance -			T

instance toInt		T
instance fromInt	T

instance toString	T
instance fromString	T
