module BinTreeMapAndFold

import StdEnv
import StdDebug

:: BTree a				= Tip a | Bin (BTree a) (BTree a)

mapbtree				:: (a -> b) (BTree a) -> BTree b
mapbtree f (Tip a)		= Tip (f a)
mapbtree f (Bin t1 t2)	= Bin (mapbtree f t1) (mapbtree f t2)

foldbtree				:: (a a -> a) (BTree a) -> a
foldbtree f (Tip a)		= a
foldbtree f (Bin t1 t2)	= f (foldbtree f t1) (foldbtree f t2)

f1						= foldbtree (+)

f2						= foldbtree (+) o (mapbtree (const 1))

f3						= foldbtree (\x y -> 1 + max x y) o (mapbtree (const 0))

f4						= foldbtree (++) o (mapbtree (\x -> [x]))

Start					:: (Int,Char,Int,Char,Int,Char,[Int],Char)
Start					= ( f1 testTree, '\n'
						  , f2 testTree, '\n'
						  , f3 testTree, '\n'
						  , f4 testTree, '\n'
						  )

testTree				= Tip 42
