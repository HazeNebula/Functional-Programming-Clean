Given:

	map :: (a -> b) [a] -> [b]
	map f []      = []                 (1)
	map f [x:xs]  = [f x : map f xs]   (2)
	
	(f o g) x     = f (g x)            (3)

Prove the following proposition for all finite lists xs and functions f, g:

	map (f o g) xs = map f (map g xs)

