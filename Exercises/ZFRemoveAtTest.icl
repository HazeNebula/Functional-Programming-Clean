module ZFRemoveAtTest

/*	Test module ZFRemoveAt
	To work with Gast:
		(*) use Environment 'Gast'
		(*) set Project Options to 'Basic Values Only'
*/
import gast
import ZFRemoveAt


Start						= testn 1000
								(\n ->    identical_to_removeAt n
								       /\ stay_empty            n
								       /\ stay_single           n
								       /\ True
								)

identical_to_removeAt		:: Int -> Property
identical_to_removeAt n		= name "identical to removeAt" 
								(removeAt n l == removeAt2 n l)
where l						= [1..100]

stay_empty					:: Int -> Property
stay_empty n				= name "stay empty"
								(isEmpty (removeAt2 n []))

stay_single					:: Int -> Property
stay_single n				= name "stay single"
								(removeAt n l == removeAt2 n l)
where l						= [42]
