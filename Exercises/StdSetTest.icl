module StdSetTest

/*	Test module StdSet
	For working with Gast:
		(*) use Environment 'Gast'
		(*) Set project options to 'Basic Values Only' and an '8M' Maximum Heap Size.
*/

import gast
import GenLexOrd
import StdSet

Start = testn 2000 
        (\n` n2` m -> let n  = cast [A,B,C] n`
                          n2 = cast [A,B,C] n2`
                       in 
							membership            m n
						/\	conversion_invariant  n
						/\	length_property       n
						/\	subset_property       n n2
						/\	strictsubset_property n n2
						/\	empty_properties      m n
						/\	disjoint_properties   n n2
						/\	union_properties      n n2

						/\	True
                   )

:: Enum = A | B | C 

derive bimap []
derive ggen Enum
derive genShow Enum
derive gEq Enum
derive gLexOrd Enum
instance == Enum where (==) x y = gEq{|*|} x y
instance <  Enum where  (<) x y = gEq{|*|} (gLexOrd{|*|} x y) LT

// clean should have something like this!
cast :: a a -> a
cast _ x = x

membership :: Enum [Enum] -> Property
membership x xs
 = name "membership"
   ( memberOfSet x s <==> isMember x xs )
  where s = toSet xs

conversion_invariant :: [Enum] -> Property
conversion_invariant xs
 = name "conversion_invariant" 
   ( toSet (fromSet xs`) == xs` )
   where xs` = toSet xs

length_property :: [Enum] -> Property
length_property xs
 = name "length_property"
   ( numberOfElements s == length (removeDup xs) )
 where s = toSet xs

subset_property :: [Enum] [Enum] -> Property
subset_property xs ys
 = name "subset_property"
   ( (isSubset u v) <==> all (flip isMember ys) xs)
  where (u,v) = (toSet xs, toSet ys)

strictsubset_property :: [Enum] [Enum] -> Property
strictsubset_property xs ys
 = name "strictsubset_property"
   ( (isStrictSubset u v) <==> (all (flip isMember ys) xs && not (all (flip isMember xs) ys)) )
  where (u,v) = (toSet xs, toSet ys)

// everything you always wanted to know about the empty set...
// ... but were afraid to ask
empty_properties :: Enum [Enum] -> Property
empty_properties x xs
 = name "empty_properties"
   ( isEmptySet (cast dummy zero) /\ isEmptySet (toSet (cast [A] [])) /\
     ((numberOfElements s == 0) <==> isEmptySet s) /\ 
           ((zero == s)     <==> isEmptySet s) )
 where s = toSet xs
       dummy :: Set Enum
       dummy = undef

union_properties :: [Enum] [Enum] -> Property
union_properties xs ys
 = name "union_properties"
   ( union u v == toSet (xs++ys) )
 where (u,v) = (toSet xs, toSet ys)

disjoint_properties :: [Enum] [Enum] -> Property
disjoint_properties xs ys
 = name "disjoint_properties"
   ( isDisjoint u v <==> (numberOfElements u + numberOfElements v == numberOfElements (union u v)) )
 where (u,v) = (toSet xs, toSet ys)
