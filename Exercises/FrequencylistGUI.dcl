definition module FrequencylistGUI

import StdEnv, StdIO

/** showFrequencylist [(a_1,f_1) ... (a_n,f_n)] world
 * shows in a bar diagram the element and their frequencies.
 * For n=0 only the axes of the graph are drawn.
 */
showFrequencylist  :: [(a,Int)] *World -> *World | toString a
showFrequencylist2 :: [(a,Int)] *World -> *World | toString a
