implementation module Trajectory

import StdEnv
import StdDebug

:: T :== Real		// T denotes time (s). This is a synonym type for Real.
:: V :== Real		// R denotes velocity (m/s).
:: A :== Real		// A denotes angle (radian).
:: G :== Real		// G denotes gravity (m/s^2).
:: D :== Real		// M denotes distance (m).

// De gravity g is a 'real' constant:
g						:: G
g						= 9.81

// the asked functions:
v_x						:: V A T -> V
v_x _ _ _ = trace_n "v_x not yet implemented" zero

v_y						:: V A T -> V
v_y _ _ _ = trace_n "v_y not yet implemented" zero

x_at					:: V A T -> D
x_at _ _ _ = trace_n "x_at not yet implemented" zero

y_at					:: V A T -> D
y_at _ _ _ = trace_n "y_at not yet implemented" zero

h						:: V A D -> D
h _ _ _ = trace_n "h not yet implemented" zero

// Determining the best angle from {0.01*pi, 0.02*pi .. 0.5*pi}:
Start					= best_angle 5.0

pi						= 3.1415926

best_angle				:: V -> A
best_angle _ = trace_n "best_angle not yet implemented" zero

