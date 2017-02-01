module ReturnAndBind

import StdEnv
import StdDebug
import Random

Start = 42

(bind1) infix 0 :: (St s a) (a -> (St s b)) -> St s b
(bind1) _ _ = abort "(bind1) not yet implemented"

sum2 :: (RandomSeed -> (Int,RandomSeed))
sum2 = abort "sum2 not yet implemented"

seqList1 :: [St s a] -> St s [a]
seqList1 _ = abort "seqList1 not yet implemented"

