module RewriteRecursion

import StdEnv
import StdDebug

/*

Start = gcd -42 0

      
Start = gcd 0 -42

Start = gcd 18 42

Start = gcd 123456789 987654321

*/

Start = (gcd -42 0, gcd 0 -42, gcd 18 42, gcd 123456789 987654321)
