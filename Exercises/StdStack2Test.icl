module StdStack2Test

/*	Test module StdStack2
	For working with Gast: 
		(*) use Environment 'Gast'
		(*) Set Project Options to 'Basic Values Only' and '8M' Maximum Heap Size
*/

import gast
import TestStdStack2

Start
							= testn 1000
								(\x y n str ->
								      stack_is_reverse    n
								   /\ stack_is_reverse2   str
								   /\ top_after_push      n x
								   /\ top_after_push2   y str
								   /\ pop_shortens_stack  n
								   /\ pop_shortens_stack2 n
								   /\ True
								)

stack_is_reverse			:: Int -> Property
stack_is_reverse n			= name "stack_is_reverse"
							  (elements (foldl (flip push) listStack [1..n`]) == reverse [1..n`])
where            n`			= min n 100

stack_is_reverse2			:: String -> Property
stack_is_reverse2 str		= name "stack_is_reverse2"
							  (elements (foldl (flip push) charStack chars) == reverse chars)
where            chars		= [c \\ c <-: str]

top_after_push				:: Int Int -> Property
top_after_push x n			= name "top_after_push"
							  (top (push x (foldl (flip push) listStack [1..n`])) == x)
where         n`			= min n 100

top_after_push2				:: Char String -> Property
top_after_push2 c str		= name "top_after_push2"
							  (top (foldl (flip push) charStack (chars ++ [c])) == c)
where            chars		= [c \\ c <-: str]

pop_shortens_stack			:: Int -> Property
pop_shortens_stack n		= name "pop_shortens_stack"
							  (length (elements (pop stack)) == length (elements stack) - 1)
where stack					= foldl (flip push) listStack [1..n`]
	  n`					= max 1 (min n 100)

pop_shortens_stack2			:: Int -> Property
pop_shortens_stack2 n		= name "pop_shortens_stack2"
							  (length (elements (pop stack)) == length (elements stack) - 1)
where stack					= foldl (flip push) charStack (repeatn n` 'a')
	  n`					= max 1 (min n 100)
