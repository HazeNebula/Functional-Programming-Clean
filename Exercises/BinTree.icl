implementation module BinTree

import StdEnv
import StdDebug

//  Binary trees
::  Tree a = Leaf | Node a (Tree a) (Tree a)

t0 :: Tree Int;		t0 = Leaf
t1 :: Tree Int;		t1 = Node 4 t0 t0
t2 :: Tree Int;		t2 = Node 2 t0 t1
t3 :: Tree Int;		t3 = Node 5 t2 t0
t4 :: Tree Int;		t4 = Node 5 t2 t2
t5 :: Tree Int;		t5 = Node 1 Leaf (Node 2 Leaf (Node 3 Leaf (Node 4 Leaf Leaf)))
t6 :: Tree Int;		t6 = Node 1 (Node 2 (Node 3 (Node 4 Leaf Leaf) Leaf) Leaf) Leaf
t7 :: Tree Int;		t7 = Node 4 (Node 1 Leaf Leaf) (Node 5 (Node 2 Leaf Leaf) Leaf)

//  2.
nodes				:: (Tree a) -> Int
nodes _ = trace_n "nodes not yet implemented" zero

leaves				:: (Tree a) -> Int
leaves _ = trace_n "leaves not yet implemented" zero

depth				:: (Tree a) -> Int
depth _ = trace_n "depth not yet implemented" zero

