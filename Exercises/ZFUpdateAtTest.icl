module ZFUpdateAtTest

/*	Test module ZFUpdateAt
	To work with Gast:
		(*) use Environment 'Gast'
		(*) set Project Options to 'Basic Values Only'
*/
import gast
import ZFUpdateAt

Start						= testn 1000
								(\m n x ->
								    let l = [1 .. n bitand 0xFF] in
								       identical_to_updateAt m l x
								    /\ True
								)

identical_to_updateAt		:: Int [Int] Int -> Property
identical_to_updateAt m l x	= name "identical to updateAt" 
									(updateAt m x l == updateAt2 m x l)
									     /\
									(ForEach [1 .. length l] (\i -> updateAt i x l == updateAt2 i x l))
