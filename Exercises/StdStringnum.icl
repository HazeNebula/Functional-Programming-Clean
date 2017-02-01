implementation module StdStringnum

import Stringnum
import StdDebug

:: Stringnum = Stringnum String

instance zero       Stringnum where zero = abort "instance zero Stringnum not yet implemented"

instance one        Stringnum where one = trace_n "instance one Stringnum not yet implemented" zero

instance <          Stringnum where < _ _ = trace_n "instance < Stringnum not yet implemented" False

instance ==         Stringnum where == _ _ = trace_n "instance == Stringnum not yet implemented" False

instance +          Stringnum where + _ _ = trace_n "instance + Stringnum not yet implemented" zero

instance -          Stringnum where - _ _ = trace_n "instance - Stringnum not yet implemented" zero

instance *          Stringnum where * _ _ = trace_n "instance * Stringnum not yet implemented" zero

instance /          Stringnum where / _ _ = trace_n "instance / Stringnum not yet implemented" zero

instance toString   Stringnum where toString _ = trace_n "instance toString Stringnum not yet implemented" ""

instance fromString Stringnum where fromString _ = trace_n "instance fromString Stringnum not yet implemented" zero

instance fromInt    Stringnum where fromInt _ = trace_n "instance fromInt Stringnum not yet implemented" zero

