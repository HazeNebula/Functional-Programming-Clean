definition module BinTree

::  Tree a = Leaf | Node a (Tree a) (Tree a)

t0		:: Tree Int
t1		:: Tree Int
t2		:: Tree Int
t3		:: Tree Int
t4		:: Tree Int
t5		:: Tree Int
t6		:: Tree Int
t7		:: Tree Int

nodes	:: (Tree a) -> Int
leaves	:: (Tree a) -> Int
depth	:: (Tree a) -> Int
