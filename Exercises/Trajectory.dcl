definition module Trajectory

:: T :== Real		// T denotes time (s). This is a synonym type for Real.
:: V :== Real		// R denotes velocity (m/s).
:: A :== Real		// A denotes angle (radian).
:: G :== Real		// G denotes gravity (m/s^2).
:: D :== Real		// M denotes distance (m).

// The gravity g
g						:: G

// The asked functions:
v_x						:: V A T -> V
v_y						:: V A T -> V
x_at					:: V A T -> D
y_at					:: V A T -> D
h						:: V A D -> D

// To determine the best angle from {0.01*pi, 0.02*pi .. 0.5*pi}:
best_angle				:: V -> A
