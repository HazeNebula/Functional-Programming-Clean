module BinSearchTreeImage

/* Instructions:

(1) copy BinTree.(i/d)cl and BinSearchTree.(i/d)cl from Practicum to
    {iTasks-SDK}\Experiments\SVG_tests\
    
(2) in these modules change the type

    :: Tree a = Node a (Tree a) (Tree a) | Leaf
    
    to
    
    :: BTree a = BLeaf | BNode a (BTree a) (BTree a)	// ORDER OF DATACONSTRUCTORS IS ESSENTIAL!!
    
    and adapt the corresponding function definitions.
    
(3) this main file (BinSearchTreeImage.icl) must be in the same folder:
    {iTasks-SDK}\Experiments\SVG_tests\
    
(4) create a new project and set de environment to 'iTasks'

(5) Bring-Up-To-Date and start generated application

(6) Open a browser and navigate to localhost.
    The application creates two tasks:
    (a) The task on the left allows you to enter subsequent elements that are inserted in the tree, one after another.
    (b) The task on the right must be finished by you by writing the function treeImage. This function must render the tree structure in such a way
        that Nodes of the same depth have the same y-coordinate, and the root having the smallest y-coordinate.
*/

import iTasks								// de algemene iTask API
import iTasks.API.Extensions.SVG.SVGlet		// specialiseer task editors
from   StdFunc import flip

import BinSearchTree						// type definitie van Tree en voorbeeldbomen z0 .. z8
derive class iTask BTree

Start				:: *World -> *World
Start world			= startEngine [publish "/" (WebApp []) (\_ -> task)] world

task				:: Task [Int]
task				= withShared [] (\sharedList ->
						(  (updateSharedInformation (Title "Edit list") [] sharedList <<@ ArrangeHorizontal)
						   -||-
						   (viewSharedInformation (Title "Tree view") [imageView treeImage` (\_ _ -> Nothing)] sharedList <<@ ArrangeHorizontal)
						) <<@ ArrangeHorizontal
					  ) <<@ FullScreen

font				= normalFontDef "Courier New" fonthoogte
fonthoogte			= 14.0

treeImage`			:: [Int] *TagSource -> Image m
treeImage` nrs tags	= treeImage (foldl (flip insertTree) BLeaf nrs) tags

treeImage			:: (BTree Int) *TagSource -> Image m
treeImage BLeaf ts	= margin (px zero,d) (circle d <@< {strokewidth=zero})
where
	d				= px (fonthoogte / 3.0)
treeImage (BNode x l r) [(tl,utl),(tr,utr):ts]
					= above (repeat AtMiddleX) [] 
					     [ textbox (toString x)
					     , margin (px zero,w_r /. 2,px zero,w_l /. 2) (beside (repeat AtTop) []
					          [ line Nothing Slash     (w_r /. 2) dy
					          , line Nothing Backslash (w_l /. 2) dy
					          ] Nothing)
					     , beside (repeat AtTop) []
					          [ tag utl (treeImage l ts_l)
					          , tag utr (treeImage r ts_r)
					          ] Nothing
					     ] Nothing
where
	(ts_l,ts_r)		= unravel ts
	(w_l, w_r)		= (imagexspan tl, imagexspan tr)
	dy				= px (fonthoogte / 2.0)

unravel				:: .[.a] -> (.[.a],.[.a])
unravel [a,b:abs]	= ([a:as],[b:bs]) where (as,bs) = unravel abs

textbox str			= overlay [(AtMiddleX,AtMiddleY)] [] [text font str]
					          (Just (rect (textxspan font str *. 1.2) (px (fonthoogte + 4.0)) <@< {fill   = toSVGColor "none"}
					                                                                          <@< {stroke = toSVGColor "black"}
					          )     )
