implementation module Map

import StdDebug
import BinTree		// for the Tree type and examples t0 up to and including t7
import Maybe        // for the Maybe type
import StdList      // for the standard map function

//	define the type constructor class Map such that the below instances can be created.

instance Map []    where Map f xs = map      f xs
instance Map Maybe where Map f mb = mapMaybe f mb
instance Map Tree  where Map f tr = mapTree  f tr

// given function, for Maybe:
mapMaybe 		        :: (a -> b) (Maybe a) -> Maybe b
mapMaybe f Nothing	    = Nothing
mapMaybe f (Just x)	    = Just (f x)

// given function, for Tree:
mapTree 			    :: (a -> b) (Tree a) -> Tree b
mapTree f Leaf		    = Leaf
mapTree f (Node x l r)	= Node (f x) (mapTree f l) (mapTree f r)
