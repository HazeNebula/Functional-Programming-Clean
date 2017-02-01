implementation module TestStdStack2

import StdEnv
import StdDebug
import StdStack2

listStack :: Stack2 a
listStack = { stack    = [], ... // define the other record fields as well

            }

charStack :: Stack2 Char
charStack = { stack    = "", ... // define the other record fields as well

            }

Start		= (elements (push "Hello"
			            (push "World!"
			            listStack))
			  ,elements (push 'C'
			            (push 'B'
			            (push 'A'
			            charStack)))
			  )
