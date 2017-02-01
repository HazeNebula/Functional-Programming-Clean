definition module ListOverloading

import StdEnv

instance zero [a] | zero a
instance one  [a] | one  a
instance ~    [a] | ~    a
instance +    [a] | +    a
instance -    [a] | -    a
instance *    [a] | *    a
instance /    [a] | /    a
