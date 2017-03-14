implementation module StdEnvExt

/** Collection of functions of more general purpose.
*/
import StdEnv
import StdMaybe

/** const2 a _ _ = a
		is a frequently occurring version of the const function.
*/
const2 :: !.a .b .c -> .a
const2 a _ _ = a

/**	iterateSt f x
		is the state based version of iterate (StdList):
	iterateSt f x_0
		[y_1,y_2,y_3,y_4...]
	where
		(y_i,x_i) = f x_{i-1}
*/
iterateSt :: !(s -> (a,s)) !s -> [a]
iterateSt f s = let (a,s1) = f s in [a:iterateSt f s1]

/** iterateStn n f x
		is the finite version of iterateSt.
*/
iterateStn :: !Int !(s -> (a,s)) !s -> (![a],!s)
iterateStn 0 _ s	= ([],s)
iterateStn n f s
	# (a, s)		= f s
	# (as,s)		= iterateStn (n-1) f s
	= ([a:as],s)

/** State strict version of seq:
*/
sseq :: ![.(.s -> .s)] !.s -> .s
sseq [f:fs] s = sseq fs (f s)
sseq []     s = s


/** State strict version of seqList:
*/
sseqList:: ![St .s .a] !.s -> (![.a],!.s)
sseqList [f:fs] s
	#! (x, s) = f s
	#! (xs,s) = sseqList fs s
	=  ([x:xs],s)
sseqList _ s
	= ([],s)

/** apply x [f_0 ... f_n] = [f_0 x, ..., f_n x]
*/
apply :: a ![a->.b] -> [.b]
apply x fs	= [f x \\ f<-fs]

/**	State passing version of map:
*/
mapSt :: !(.(.a,.s) -> .(.b,.s)) !(![.a],.s) -> (![.b],.s)
mapSt f ([],s)
	= ([],s)
mapSt f ([a:as],s)
	# (b, s)	= f (a,s)
	# (bs,s)	= mapSt f (as,s)
	= ([b:bs],s)

/**	Strict state passing version of map:
*/
smapSt :: !(.(.a,.s) -> .(.b,.s)) !(![.a],!.s) -> (![.b],!.s)
smapSt f ([],s)
	= ([],s)
smapSt f ([a:as],s)
	#! (b, s)	= f (a,s)
	#! (bs,s)	= smapSt f (as,s)
	= ([b:bs],s)

/**	singleOutElems [a_1..a_n] = [(a_1,[a_2..a_n]),(a_2,[a_1,a_3..a_n])..(a_n,[a_1..a_{n-1}])]
*/
singleOutElems :: ![a] -> [(a,[a])]
singleOutElems as
	= singleOut [] as
where
	singleOut :: [a] [a] -> [(a,[a])]
	singleOut _ [] = []
	singleOut prefix [a:as]
		= [(a,prefix++as) : singleOut (prefix++[a]) as]

/**	hdtl [a:as] = (a,as)
*/
hdtl :: ![a] -> (a,[a])
hdtl [a:as] = (a,as)

/** isSingleton [_] = True;
	isSingleton  _  = False.
*/
isSingleton :: ![a] -> Bool
isSingleton [_]		= True
isSingleton  _		= False


/** filterSt cond xs st
		filters all elements from xs using a state parameter st that is threaded along.
*/
filterSt     :: (a .s -> (Bool,.s)) !.[a] !.s -> (.[a],.s)
filterSt cond [] s
	= ([],s)
filterSt cond [x:xs] s
# (b,  s)		= cond x s
# (yes,s)		= filterSt cond xs s
| b				= ([x:yes],s)
| otherwise		= (   yes, s)

/**	spanfilter cond xs = (filter cond xs, filter (not o cond) xs)
*/
spanfilter :: (a -> Bool) !.[a] -> (.[a],.[a])
spanfilter cond []
	= ([],[])
spanfilter cond [x:xs]
	| cond x	= ([x:yes],no)
	| otherwise	= (yes,[x:no])
where
	(yes,no)	= spanfilter cond xs

spanfilterSt :: (a .s -> (Bool,.s)) !.[a] .s -> (.(.[a],.[a]),.s)
spanfilterSt cond [] s
	= (([],[]),s)
spanfilterSt cond [x:xs] s
	# (ok,s)		= cond x s
	# ((yes,no),s)	= spanfilterSt cond xs s
	| ok			= (([x:yes],no),s)
	| otherwise		= ((yes,[x:no]),s)

/**	find1 cond (A ++ [a] ++ B) = a
	where for each x in A: not (cond x) /\ cond a
*/
find1 :: !(a -> Bool) ![a] -> a
find1 c as			= case filter c as of
						[a:_]	= a
						none	= abort "find1: no elements found.\n"

/** break cond (A ++ B ++ C) = (A,B,C)
	where for each x in A: not cond x     /\
	      for each x in B:     cond x     /\
	      if C=[x:_]:      not cond x
*/
break :: !(a -> Bool) ![a] -> (![a],![a],![a])
break c xs
	# (no,yes)	= span (not o c) xs
	# (yes,no`)	= span c yes
	= (no,yes,no`)

/** break1 cond (A ++ [B] ++ C) = (A,B,C)
	where for each x in A: not cond x     /\ 
	                           cond B     /\ 
	      if C=[x:_]:      not cond x
*/
break1 :: !(a -> Bool) ![a] -> (![a],!a,![a])
break1 c xs
	= case break c xs of
		(a,[b],c)	= (a,b,c)
		(a,b,c)		= abort ("break1: [B] is of length: " <+++ length b <+++ "\n")

/** unbreak (a,b,c) = a ++ b ++ c
*/
unbreak :: !(![a],![a],![a]) -> [a]
unbreak (a,b,c) = a ++ b ++ c


/** unbreak1 (a,b,c) = a ++ [b] ++ c
*/
unbreak1 :: !(![a],!a,![a]) -> [a]
unbreak1 (a,b,c) = a ++ [b] ++ c

/** [a_1..x..a_n] x = i
	where
		a_j <> x for all j<i
		a_j == x for j==i
*/
(??) infixl 9 :: ![a] !a -> Int | == a
(??) ys x = search ((==) x) ys 0

(???) infixl 9 :: ![a] !(a -> Bool) -> Int
(???) ys c = search c ys 0

search :: !(a -> Bool) ![a] !Int -> Int
search _ [] _	= -1
search c [y:ys] i
	| c y		= i
	| otherwise	= search c ys (i+1)

/**	weave [a_1..a_n] [b_1..b_m]
		= [a_1,b_1, a_2,b_2, ... a_k,b_k] with k = min(m,n)
*/
weave :: ![a] [a] -> [a]
weave [a:as] [b:bs] = [a,b:weave as bs]
weave _      _		= []

/** unweave [a_1,a_2..a_n]
		= ([a_1,a_3..],[a_2,a_4..])
*/
unweave :: ![a] -> ([a],[a])
unweave [x,y:zs]	= ([x:xs],[y:ys])
where
	(xs,ys)			= unweave zs
unweave [x]			= ([x],[])
unweave []			= ([],[])

/** unweave_n n [a_1..a_n, a_{n+1}..a_{2n} ..]
		= [[a_1,a_{n+1}..],[a_2,a_{n+2}..] ..]
*/
unweave_n :: !Int [a] -> [[a]]
unweave_n nrLists zs 
	| length first_n < nrLists
		= repeatn nrLists []
	| otherwise
		= glue first_n (unweave_n nrLists after_n)
where
	(first_n,after_n)	= splitAt nrLists zs
	
	glue :: ![a] [[a]] -> [[a]]		// must be non-strict in its second argument in order to work for streams
	glue []     _	= []
	glue [a:as] xss	= [[a:hd xss]:glue as (tl xss)]


/** Immediate instances of toString for (,) and (,,)
*/
instance toString (a,b)     | toString a & toString b              where toString (a,b)    = "(" <+++ a <+++ "," <+++ b <+++ ")"
instance toString (a,b,c)   | toString a & toString b & toString c where toString (a,b,c)  = "(" <+++ a <+++ "," <+++ b <+++ "," <+++ c <+++ ")"
instance toString (Maybe a) | toString a                           where toString (Just a) = "(Just " <+++ a <+++ ")"
                                                                         toString nothing  = "Nothing"

/**	Useful string concatenation function
*/
(<+++) infixl :: !String !a -> String | toString a
(<+++) str x = str +++ toString x

(+++>) infixr :: !a !String -> String | toString a
(+++>) x str = toString x +++ str


/** showList  inf   [x_0 ... x_n] = "<x_0><inf>...<inf><x_n>"
	showListF inf f [x_0 ... x_n] = "<f x_0><inf>...<inf><f x_n>"
*/
showList :: !String !.[a] -> String | toString a
showList inf []			= ""
showList inf [x]		= toString x
showList inf [x:xs]		= x +++> inf +++> showList inf xs

showListF :: !String !(a -> String) !.[a] -> String
showListF inf f []		= ""
showListF inf f [x]		= f x
showListF inf f [x:xs]	= f x +++> inf +++> showListF inf f xs


/** lookup k [...(k,v)...] = Just v
	lookup k _             = Nothing
*/
lookup :: !k !(AssocList k v) -> Maybe v | Eq k
lookup k assocl
	= case [v \\ (k`,v)<-assocl | k==k`] of
		[v:_]	= Just v
		_		= Nothing

/** lookup _ k [...(k,v)...] = v
	lookup v k _             = v
*/
lookupd :: v !k !(AssocList k v) -> v | Eq k
lookupd v k assocl
	= case [v` \\ (k`,v`)<-assocl | k==k`] of
		[v`:_]	= v`
		_		= v

/** keymember k [...(k,v)...] = True
	keymember _ _             = False
*/
keymember :: !k !(AssocList k v) -> Bool | Eq k
keymember k assocl
	= isJust (lookup k assocl)

/** addkeyvalue (k,v) [...(k,_)...] = [...(k,v)...]
	addkeyvalue _     assocl        = assocl ++ [(k,v)]
*/
addkeyvalue :: !(!k,v) !(AssocList k v) -> AssocList k v | Eq k
addkeyvalue (k,v) assocl
	= case span (\(k`,_) -> k<>k`) assocl of
		(before,[_:after])	= before ++ [(k,v):after]
		(before,empty)		= before ++ [(k,v)]

/** updkeyvalue k f [...(k,v)...] = [...(k,f v)...]
	updkeyvalue _ _ assocl        = assocl
*/
updkeyvalue :: !k !(v -> v) !(AssocList k v) -> AssocList k v | Eq k
updkeyvalue k f assocl
	= case span (\(k`,_) -> k<>k`) assocl of
		(before,[(k,v):after])	= before ++ [(k,f v):after]
		(before,empty)			= before

/** deletekeyvalue k [...(k,v)...] = [... ...]
	deletekeyvalue _ assocl        = assocl
*/
deletekeyvalue :: !k !(AssocList k v) -> AssocList k v | Eq k
deletekeyvalue k assocl
	= case span (\(k`,_) -> k<>k`) assocl of
		(before,[_:after])		= before ++ after
		(before,empty)			= before

/** isAllMember xs ys is true iff all elements of xs are member of ys.
*/
isAllMember :: ![a] [a] -> Bool | Eq a
isAllMember xs ys = and (map (\x -> isMember x ys) xs)

/** zipWith f as bs = [f a_0 b_0, f a_1 b_1, ..., f a_n b_n]
*/
zipWith :: (a b -> c) ![a] ![b] -> [c]
zipWith f as bs = [f a b \\ a<-as & b<-bs]

/** setbetween x low up
		returns x   iff low <= x <= up
		returns low iff low > x
		returns up  iff x > up
*/
setbetween :: !a !a !a -> a | Ord a
setbetween x low up
	| low > x	= low
	| x > up	= up
	| otherwise	= x

/** isbetween x low up
		returns True iff low <= x <= up
*/
isbetween :: !a !a !a -> Bool | Ord a
isbetween x low up
	= low <= x && x <= up

/** minmax (a,b) = (a,b) if a<=b; (b,a) otherwise
*/
minmax :: !(!a,!a) -> (!a,!a) | Ord a
minmax (a,b)
	| a<=b		= (a,b)
	| otherwise	= (b,a)


/** swap (a,b) = (b,a)
*/
swap :: !(.a,.b) -> (.b,.a)
swap (a,b) = (b,a)

/** modulo int
*/
instance mod Int where
	(mod) a b	= a - b * (a/b)

/**	foldl1 f xs folds f to the left over non-empty list xs.
*/
foldl1 :: !(a -> a -> a) ![a] -> a
foldl1 f [x : xs]	= foldl f x xs

/** foldr1 f xs folds f to the right over non-empty list xs.
*/
foldr1 :: !(a -> a -> a) ![a] -> a
foldr1 f [x]	= x
foldr1 f [x:xs]	= f x (foldr1 f xs)
		
removeQuotes :: !{#Char} -> String
removeQuotes "" = ""
removeQuotes s = removeQuotes` s 0
where
	removeQuotes` :: !{#Char} Int -> String
	removeQuotes` s i	
		| i == size s
			= ""
		| otherwise
			# c = select s i
			| c == '\"' || c == '\\'
				= removeQuotes` s (inc i)
			| otherwise
				= toString c +++ removeQuotes` s (inc i)
				
replaceInString :: !String !String !String -> String
replaceInString toReplace replacement s  
	# result = replaceInString` (fromString toReplace) (fromString replacement) (fromString s)
	= charlist2string "" result
where
 	replaceInString` :: ![Char] ![Char] ![Char] -> [Char]
 	replaceInString` toReplace replacement []	= []
 	replaceInString` toReplace replacement s=:[x:xs]	
 		| length toReplace > length s	= s
 		# firstPart = take (length toReplace) s
 		# lastPart = drop (length toReplace) s
 		| firstPart == toReplace 	= replacement ++ replaceInString` toReplace replacement lastPart
 		| otherwise					= [x:replaceInString` toReplace replacement xs]
 	
	charlist2string :: !String ![Char] -> String
	charlist2string str []	= str
	charlist2string str [x:xs]	= charlist2string (str+++toString x) xs

/** stringStarts s1 prefix yields true iff s1 = prefix +++ s for some s.
*/
stringStarts :: !String !String -> Bool
stringStarts s1 prefix = size s1 >= size prefix && s1%(0,size prefix-1) == prefix

/** removePrefix str prefix yields (Just s) iff str = prefix +++ s, and Nothing otherwise.
*/
removePrefix :: !String !String -> Maybe String
removePrefix s1 prefix = if (stringStarts s1 prefix) (Just (s1%(size prefix,size s1-1))) Nothing

/** isSorted [x_0..x_n] holds iff x_i <= x_{i+1} for each x_i in [x_0..x_{n-1}].
*/
isSorted :: ![a] -> Bool | Ord a
isSorted []				= True
isSorted xs				= and [x <= y \\ x <- xs & y <- tl xs]

/** perhaps p Nothing = False, and perhaps p (Just a) = p a
*/
perhaps :: !(a -> Bool) !(Maybe a) -> Bool
perhaps _ Nothing		= False
perhaps p (Just a)		= p a

/** instance ~ Bool is not
*/
instance ~ Bool where (~) b		= not b

/** instance fromString Int = toInt
*/
instance fromString Int where fromString str = toInt str

/**	test and access functions for choice values:
*/
::	ThisOrThat a b	= This a | That b

isThis :: !(ThisOrThat a b) -> Bool
isThis (This _)		= True
isThis _			= False

isThat :: !(ThisOrThat a b) -> Bool
isThat (That _)		= True
isThat _			= False

this :: !(ThisOrThat a b) -> a
this (This a)		= a
this _				= abort "this [StdEnvExt]: applied to That pattern instead of This pattern."

that :: !(ThisOrThat a b) -> b
that (That b)		= b
that _				= abort "that [StdEnvExt]: applied to This pattern instead of That pattern."

instance sinus      Real where sinus      x = sin  x
instance cosinus    Real where cosinus    x = cos  x
instance tangens    Real where tangens    x = tan  x
instance arcsinus   Real where arcsinus   a = asin a
instance arccosinus Real where arccosinus a = acos a
instance arctangens Real where arctangens a = atan a
