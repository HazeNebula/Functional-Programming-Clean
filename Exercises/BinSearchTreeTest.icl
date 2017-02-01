module BinSearchTreeTest

/*	Test module BinSearchTree
	To use Gast: 
		(*) set Environment to 'Gast'
		(*) set Project Options to 'Basic Values Only'
*/

import gast
import BinSearchTree

Start								= testnm 1000 1
										(\(Keys xs) -> let n` = hd` 0 xs in
										      random_balanced n`
										   /\ random_ordered  n`
										   /\ bst_ordered     xs
										   /\ True
										)
:: Keys = Keys [Int]

ggen{|Keys|} n rnd = map (\xs->Keys [ (abs x+n) rem 100 \\ x<-xs & n <- [0,7..]]) (ggen{|*|} n rnd)
genShow{|Keys|} sep p (Keys x) rest = genShow{|*|} sep p x rest

hd` :: a [a] -> a
hd` _ [x:_] = x
hd` x _     = x

bst_ordered						:: [Int] -> Property
bst_ordered src					= name "bst_ordered"
									  (ordening_komt_overeen t)
where    t						= chain_tree src

random_ordered					:: Int -> Property
random_ordered n				= name "random_ordered"
									  (ordening_komt_overeen t)
where    t						= lego_tree n

random_balanced					:: Int -> Property
random_balanced n				= name "random_balanced"
									  (is_balanced t == is_balanced2 t)
where    t						= lego_tree n

// Sommige mensen maken slimmere functies dan "sort (naarLijst boom) == naarLijst boom"; welke implementatie
// onjuist is als keys niet uniek hoeven te zijn (zoals in de opdracht staat). Merk op dat de voorbeelduitwerking 
// dus niet door deze test komt als je de laatste (<=) verandert in (<). 
//
// Omdat een binaire zoekboom met meerdere keys niet zo nuttig is, kunnen we hier enigszins laks mee omgaan. 
// - marc
ordening_komt_overeen			:: (Tree Int) -> Property
ordening_komt_overeen t 		= (is_ordered2 (<) (<) t ==> is_ordered t) /\
								  (is_ordered t ==> is_ordered2 (<=) (<=) t)

//Referentie-implementaties van gevraagde functies uit uitwerking
is_ordered2						:: (a a->Bool) (a a->Bool) (Tree a) -> Bool | Ord a
is_ordered2 lcmp rcmp boom		= is_ordered` (const True) (const True) boom
where
	is_ordered` _ _ Leaf 
      = True
	is_ordered` minOK maxOK (Node x l r) 
	  = minOK x && maxOK x && is_ordered` (flip lcmp x) maxOK l && is_ordered` minOK (rcmp x) r

::  Diepte :== Int

is_balanced2					:: (Tree a) -> Bool
is_balanced2 t					= snd (is_balanced` t)
where
    is_balanced`				:: (Tree a) -> (Diepte,Bool)
    is_balanced` Leaf			= ( 0, True )
    is_balanced` (Node _ l r)	= ( 1 + max h_l h_r, abs (h_l - h_r) <= 1 && ok_l && ok_r )
    where
        (h_l,ok_l)				= is_balanced` l
        (h_r,ok_r)				= is_balanced` r


//	Functie om semi-random bomen te bouwen uit BinTree:
trees							=: [t0,t1,t2,t3,t4,t5,t6,t7]
nr_of_trees						=: length trees

lego_tree						:: Int -> Tree Int
lego_tree 0						= t0
lego_tree n						= glue n (trees !! (n rem nr_of_trees)) (lego_tree (n/10))
where
	glue						:: !Int !(Tree Int) !(Tree Int) -> Tree Int
	glue n t Leaf				= t
	glue n t (Node x l r)
	| isEven n					= Node x (glue (n/10) t l) r
	| otherwise					= Node x l (glue (n/10) t r)

// Functie om bomen te maken waarbij elke knoop unair is; levert effectieve tegenvoorbeelden op
// in foutieve implementaties van is_ordered
chain_tree						:: [Int] -> Tree Int
chain_tree seed					= chain_tree (removeDup seed)
where
    chain_tree []				= Leaf
    chain_tree [x]				= Node x Leaf Leaf
    chain_tree [x:y:xs]			= if (y < x) (Node x (chain_tree [y:xs]) Leaf) (Node x Leaf (chain_tree [y:xs]))

numbers							:: Int -> [Int]
numbers n						= takeWhile ((<>) 0) (map (flip (rem) 100) (iterate (flip (/) -100) n))
