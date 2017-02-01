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

2. 
Prove:
	for every function f, for every finite list xs:
	
		flatten (map (map f) xs) = map f (flatten xs)

