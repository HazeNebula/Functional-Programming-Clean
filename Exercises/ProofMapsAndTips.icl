Given:

:: BTree a = Tip a | Bin (BTree a) (BTree a)

map :: (a -> b) [a] -> [b]
map f []                = []                                    (1.)
map f [x:xs]            = [f x : map f xs]                      (2.)

mapbtree :: (a -> b) (BTree a) -> BTree b
mapbtree f (Tip a)      = Tip (f a)                             (3.)
mapbtree f (Bin t1 t2)  = Bin (mapbtree f t1) (mapbtree f t2)   (4.)

foldbtree :: (a a -> a) (BTree a) -> a
foldbtree f (Tip x)     = x                                     (5.)
foldbtree f (Bin t1 t2) = f (foldbtree f t1) (foldbtree f t2)   (6.)

tips :: (BTree a) -> [a]
tips t                  = foldbtree (++) (mapbtree unit t)      (7.)

unit :: a -> [a]
unit x                  = [x]                                   (8.)

Prove:
	For all functions f, finite trees t:
	
		map f (tips t) = tips (mapbtree f t)

