definition module RandomNumbers

import Random

random_n	:: Int RandomSeed -> ([Int],RandomSeed)
random_inf	::     RandomSeed ->  [Int]
shuffle		:: [a] RandomSeed ->  [a]
