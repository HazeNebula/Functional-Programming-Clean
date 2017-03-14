definition module StdEnvExt

/** Collection of generally useful functions that are not related to SoccerFun.
*/
import StdEnv
import StdMaybe

/** const2 a _ _ = a
		is a frequently occurring version of the const function.
*/
const2 :: !.a .b .c -> .a

/**	iterateSt f x
		is the state based version of iterate (StdList):
	iterateSt f x_0
		[y_1,y_2,y_3,y_4...]
	where
		(y_i,x_i) = f x_i
*/
iterateSt :: !(s -> (a,s)) !s -> [a]

/** iterateStn n f x
		is the finite version of iterateSt.
*/
iterateStn :: !Int !(s -> (a,s)) !s -> (![a],!s)

/** State strict versions of seq and seqList:
*/
sseq    :: ![.(.s -> .s)] !.s -> .s
sseqList:: ![St .s .a]    !.s -> (![.a],!.s)

/** apply x [f_0 ... f_n] = [f_0 x, ..., f_n x]
*/
apply :: a ![a->.b] -> [.b]

/**	State passing version of map:
*/
mapSt	:: !(.(.a,.s) -> .(.b,.s)) !(![.a],.s) -> (![.b],.s)

/**	Strict state passing version of map:
*/
smapSt :: !(.(.a,.s) -> .(.b,.s)) !(![.a],!.s) -> (![.b],!.s)

/**	singleOutElems [a_1..a_n] = [(a_1,[a_2..a_n]),(a_2,[a_1,a_3..a_n])..(a_n,[a_1..a_(n-1)])]
*/
singleOutElems :: ![a] -> [(a,[a])]

/**	hdtl [a:as] = (a,as)
*/
hdtl :: ![a] -> (a,[a])

/** isSingleton [_] = True;
	isSingleton  _  = False.
*/
isSingleton :: ![a] -> Bool

/** filterSt cond xs st
		filters all elements from xs using a state parameter st that is threaded along.
*/
filterSt     :: (a .s -> (Bool,.s)) !.[a] !.s -> (.[a],.s)

/**	spanfilter cond xs = (filter cond xs, filter (not o cond) xs)
	spanfilterSt cond xs st
		same, but with state parameter st that is threaded along.
*/
spanfilter   :: (a -> Bool) !.[a] -> (.[a],.[a])
spanfilterSt :: (a .s -> (Bool,.s)) !.[a] .s -> (.(.[a],.[a]),.s)

/**	find1 cond (A ++ [a] ++ B) = a
	where for each x in A: not (cond x) /\ cond a
*/
find1 :: !(a -> Bool) ![a] -> a

/** break cond (A ++ B ++ C) = (A,B,C)
	where for each x in A: not (cond x)     /\
	      for each x in B:     (cond x)     /\
	      if C=[x:_]:      not (cond x)
*/
break :: !(a -> Bool) ![a] -> (![a],![a],![a])

/** break1 cond (A ++ [B] ++ C) = (A,B,C)
	where for each x in A: not (cond x)     /\ 
	                           (cond B)     /\ 
	      if C=[x:_]:      not (cond x)
*/
break1 :: !(a -> Bool) ![a] -> (![a],!a,![a])

/** unbreak (a,b,c) = a ++ b ++ c
*/
unbreak :: !(![a],![a],![a]) -> [a]

/** unbreak1 (a,b,c) = a ++ [b] ++ c
*/
unbreak1 :: !(![a],!a,![a]) -> [a]

/** [a_1..x..a_n] ?? x = i
	where
		a_j <> x for all j<i
		a_j == x for j==i
	
	[a_1..x..a_n] ??? c = i
	where
		not (c a_j) for all j<i
		    (c a_j) for j==i
*/
(??) infixl 9 :: ![a] !a -> Int | == a
(???)infixl 9 :: ![a] !(a -> Bool) -> Int

/**	weave [a_1..a_n] [b_1..b_m]
		= [a_1,b_1, a_2,b_2, ... ]
*/
weave :: ![a] [a] -> [a]

/** unweave [a_1,a_2..a_n]
		= ([a_1,a_3..],[a_2,a_4..])
*/
unweave :: ![a] -> ([a],[a])

/** unweave_n n [a_1..a_n, a_{n+1}..a_{2n} ..]
		= [[a_1,a_{n+1}..],[a_2,a_{n+2}..] ..]
*/
unweave_n :: !Int [a] -> [[a]]

/** Immediate instances of toString for (,), (,,) and Maybe
*/
instance toString (a,b)     | toString a & toString b
instance toString (a,b,c)   | toString a & toString b & toString c
instance toString (Maybe a) | toString a

/**	Useful string concatenation function
*/
(<+++) infixl :: !String !a -> String | toString a
(+++>) infixr :: !a !String -> String | toString a

/** showList  inf   [x_0 ... x_n] = "<x_0><inf>...<inf><x_n>"
	showListF inf f [x_0 ... x_n] = "<f x_0><inf>...<inf><f x_n>"
*/
showList	:: !String                !.[a] -> String | toString a
showListF	:: !String !(a -> String) !.[a] -> String

/** Association lists a la Haskell.
*/
::	AssocList k v :== [ (!k,v)]

/** lookup k [...(k,v)...] = Just v
	lookup k _             = Nothing
*/
lookup :: !k !(AssocList k v) -> Maybe v | Eq k

/** lookup _ k [...(k,v)...] = v
	lookup v k _             = v
*/
lookupd :: v !k !(AssocList k v) -> v | Eq k

/** keymember k [...(k,v)...] = True
	keymember _ _             = False
*/
keymember :: !k !(AssocList k v) -> Bool | Eq k

/** addkeyvalue (k,v) [...(k,_)...] = [...(k,v)...]
	addkeyvalue _     assocl        = assocl ++ [(k,v)]
*/
addkeyvalue :: !(!k,v) !(AssocList k v) -> AssocList k v | Eq k

/** updkeyvalue k f [...(k,v)...] = [...(k,f v)...]
	updkeyvalue _ _ assocl        = assocl
*/
updkeyvalue :: !k !(v -> v) !(AssocList k v) -> AssocList k v | Eq k

/** deletekeyvalue k [...(k,v)...] = [... ...]
	deletekeyvalue _ assocl        = assocl
*/
deletekeyvalue :: !k !(AssocList k v) -> AssocList k v | Eq k

/** isAllMember xs ys is true iff all elements of xs are member of ys.
*/
isAllMember :: ![a] [a] -> Bool | Eq a

/** zipWith f as bs = [f a_0 b_0, f a_1 b_1, ..., f a_n b_n]
*/
zipWith :: (a b -> c) ![a] ![b] -> [c]

/** setbetween x low up
		returns x   iff low <= x <= up
		returns low iff low > x
		returns up  iff x > up
*/
setbetween :: !a !a !a -> a | Ord a

/** isbetween x low up
		returns True iff low <= x <= up
*/
isbetween :: !a !a !a -> Bool | Ord a

/** minmax (a,b) = (a,b) if a<=b; (b,a) otherwise
*/
minmax :: !(!a,!a) -> (!a,!a) | Ord a

/** swap (a,b) = (b,a)
*/
swap :: !(.a,.b) -> (.b,.a)

/** modulo Int
*/
instance mod Int

/**	foldl1 f xs folds f to the left over non-empty list xs.
*/
foldl1 :: !(a -> a -> a) ![a] -> a

/** foldr1 f xs folds f to the right over non-empty list xs.
*/
foldr1 :: !(a -> a -> a) ![a] -> a

/*	removeQuotes str removes all quotes and slashes from str.
*/
removeQuotes :: !{#Char} -> String

/** stringStarts str prefix yields true iff str = prefix +++ s for some s.
*/
stringStarts :: !String !String -> Bool

/** removePrefix str prefix yields (Just s) iff str = prefix +++ s, and Nothing otherwise.
*/
removePrefix :: !String !String -> Maybe String

replaceInString :: !String !String !String -> String

/** other a yields the only other value of a domain that consists of two values.
*/
class other a :: !a -> a

/** isSorted [x_0..x_n] holds iff x_i <= x_{i+1} for each x_i in [x_0..x_{n-1}].
*/
isSorted :: ![a] -> Bool | Ord a

/** perhaps p Nothing = False, and perhaps p (Just a) = p a
*/
perhaps :: !(a -> Bool) !(Maybe a) -> Bool

/**	instance ~ Bool = not
*/
instance ~ Bool

/** instance fromString Int = toInt
*/
instance fromString Int

/** better class definitions of the trigonometry functions:
*/
class sinus      a :: !a -> Real
class cosinus    a :: !a -> Real
class tangens    a :: !a -> Real
class arcsinus   a :: !Real -> a
class arccosinus a :: !Real -> a
class arctangens a :: !Real -> a

instance sinus      Real
instance cosinus    Real
instance tangens    Real
instance arcsinus   Real
instance arccosinus Real
instance arctangens Real
