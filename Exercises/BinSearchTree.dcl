definition module BinSearchTree

import StdClass
import BinTree

z0				:: Tree Int
z1				:: Tree Int
z2				:: Tree Int
z3				:: Tree Int
z4				:: Tree Int
z5				:: Tree Int
z6				:: Tree Int
z7				:: Tree Int
z8				:: Tree Int

insertTree		:: a (Tree a) -> Tree a | Ord a
deleteTree		:: a (Tree a) -> Tree a | Eq, Ord a

is_ordered      :: (Tree a) -> Bool | Eq, Ord a
is_balanced     :: (Tree a) -> Bool
