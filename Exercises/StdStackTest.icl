module StdStackTest

/*	Test module StdStack
	For working with Gast:
		(*) use Environment 'Gast'
		(*) Set Project Options to 'Basic Values Only' and a '2M' Maximum Heap Size
*/

import gast
import StdStack

Start
							= testn 1000
								(\x n ->
								      newStack_is_empty
								   /\ stack_is_reverse   n
								   /\ pop_empty_is_ok
								   /\ top_after_push     n x
								   /\ pop_after_push       x
								   /\ count_counts       n x
								   /\ pop_shortens_stack n
								   /\ True
								)

newStack_is_empty			:: Property
newStack_is_empty			= name "newStack_is_empty" (isEmpty (elements empty))

stack_is_reverse			:: Int -> Property
stack_is_reverse n			= name "stack_is_reverse"
							  (elements (pushes [1..n`] newStack) == reverse [1..n`])
where            n`			= min (abs n) 100

pop_empty_is_ok				:: Property
pop_empty_is_ok				= name "pop_empty_is_ok" (count (pop empty) == 0)

top_after_push				:: Int Int -> Property
top_after_push x n			= name "top_after_push"
							  (top (push x (pushes [1..n`] newStack)) == x)
where         n`			= min (abs n) 100

pop_after_push				:: Int -> Property
pop_after_push a			= name "pop_after_push"
                              (top (pop (pop (pushes [a,b,c] newStack))) == a)
where b						= a + a + one
      c						= b + a + one

count_counts				:: Int Int -> Property
count_counts n x			= name "count_counts"
							  (length (elements stack) == count stack)
where stack					= pushes [1..n`] newStack
      n`					= min (abs n) 100

pop_shortens_stack			:: Int -> Property
pop_shortens_stack n		= name "pop_shortens_stack"
							  (count stack == 0 || count (pop stack) == count stack - 1)
where stack					= pushes [1..n`] newStack
	  n`					= min (abs n) 100

empty						:: Stack Int
empty						= newStack
