//Coded by Niels van Nistelrooij (s4713648) and Japser van den Bogart (s4781686)

Given:

(++) :: [a] [a] -> [a]
(++) []     xs = xs                (1)
(++) [y:ys] xs = [y : ys ++ xs]    (2)

map :: (a -> b) [a] -> [b]
map f []       = []                (3)
map f [x:xs]   = [f x : map f xs]  (4)

flatten :: [[a]] -> [a]
flatten []     = []                (5)
flatten [x:xs] = x ++ (flatten xs) (6)

1. 
Prove:
	For every function f, finite list as and bs:
		
		map f (as ++ bs) = (map f as) ++ (map f bs)
		
Proof by induction on as
Base case: assume as = []
	Prove: 	map f (as ++ bs) 	= (map f as) ++ (map f bs)
	Proof: 	map f (as ++ bs) 	= (map f as) ++ (map f bs)
			map f ([] ++ bs) 	= (map f []) ++ (map f bs)
			map f bs			= (map f []) ++ (map f bs)		(1)
			map f bs			= [] ++ (map f bs)				(3)
			map f bs			= map f bs						(1)
Induction case: assume property holds for certain as: map f (as ++ bs) = (map f as) ++ (map f bs)	(IH)
	Prove:	map f ([a:as] ++ bs) 		= (map f [a:as]) ++ (map f bs)
	Proof: 	map f ([a:as] ++ bs) 		= (map f [a:as]) ++ (map f bs)
			map f ([a:as] ++ bs)		= [f a : (map f as)] ++ (map f bs)							(4)
			[f a : map f (as ++ bs)]	= [f a : (map f as)] ++ (map f bs)							(2)
			[f a : (map f (as ++ bs))]	= [f a : (map f as) ++ (map f bs)]							(2)
			[f a : (map f (as ++ bs))]	= [f a : (map f (as ++ bs))]								(IH)
	

2. 
Prove:
	for every function f, for every finite list xs:
	
		flatten (map (map f) xs) = map f (flatten xs)
		
Proof by induction on xs
Base case: assume xs = []
	Prove: 	flatten (map (map f) xs)	= map f (flatten xs)
	Proof: 	flatten (map (map f) xs)	= map f (flatten xs)
			flatten (map (map f) [])	= map f (flatten [])
			flatten (map (map f) [])	= map f []									(5)
			flatten (map (map f) [])	= []										(3)
			flatten []					= []										(3)
			[]							= []										(5)
Induction case: assume property holds for certain xs: flatten (map (map f) xs) = map f (flatten xs)	(IH)
	Prove: 	flatten (map (map f) [x:xs])				= map f (flatten [x:xs])
	Proof: 	flatten (map (map f) [x:xs])				= map f (flatten [x:xs])
			flatten [(map f) x : map (map f) xs]		= map f (flatten [x:xs])					(4)
			flatten [(map f) x : map (map f) xs]		= map f (x ++ (flatten xs))					(6)
			flatten [(map f) x : map (map f) xs]		= (map f x) ++ (map f (flatten xs))			(Previous proof)
			((map f) x) ++ (flatten (map (map f) xs))	= (map f x) ++ (map f (flatten xs))			(6)
			((map f) x) ++ (flatten (map (map f) xs))	= (map f x) ++ (flatten (map (map f) xs))	(IH)
			(map f x) ++ (flatten (map (map f) xs))		= (map f x) ++ (flatten (map (map f) xs))	
			
	
		

