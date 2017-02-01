implementation module StdStringnum2

import Stringnum2
import StdDebug

:: Stringnum2 = Stringnum2 String

instance zero       Stringnum2 where zero = abort "instance zero Stringnum2 not yet implemented"

instance one        Stringnum2 where one = trace_n "instance one Stringnum2 not yet implemented" zero

instance abs        Stringnum2 where abs _ = trace_n "instance abs Stringnum2 not yet implemented" zero

instance ~          Stringnum2 where ~ _ = trace_n "instance ~ Stringnum2 not yet implemented" zero

instance sign       Stringnum2 where sign _ = trace_n "instance sign Stringnum2 not yet implemented" zero

instance <          Stringnum2 where < _ _ = trace_n "instance < Stringnum2 not yet implemented" False

instance ==         Stringnum2 where == _ _ = trace_n "instance == Stringnum2 not yet implemented" False

instance +          Stringnum2 where + _ _ = trace_n "instance + Stringnum2 not yet implemented" zero

instance -          Stringnum2 where - _ _ = trace_n "instance - Stringnum2 not yet implemented" zero

instance *          Stringnum2 where * _ _ = trace_n "instance * Stringnum2 not yet implemented" zero

instance /          Stringnum2 where / _ _ = trace_n "instance / Stringnum2 not yet implemented" zero

instance toString   Stringnum2 where toString _ = trace_n "instance toString Stringnum2 not yet implemented" ""

instance fromString Stringnum2 where fromString _ = trace_n "instance fromString Stringnum2 not yet implemented" zero

instance fromInt    Stringnum2 where fromInt _ = trace_n "instance fromInt Stringnum2 not yet implemented" zero

