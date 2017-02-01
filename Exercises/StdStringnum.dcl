definition module StdStringnum

import StdEnv

:: Stringnum = Stringnum String

instance zero       Stringnum
instance one        Stringnum
instance <          Stringnum
instance ==         Stringnum
instance +          Stringnum
instance -          Stringnum
instance *          Stringnum
instance /          Stringnum
instance toString   Stringnum
instance fromString Stringnum
instance fromInt    Stringnum
