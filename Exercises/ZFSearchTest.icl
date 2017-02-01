module ZFSearchTest

/*	Test module ZFZoek
	For working with Gast:
		(*) use Environment 'Gast'
		(*) set Project Options set 'Basic Values Only'
*/
import gast
import ZFSearch

Start							= testn 1000
									(\m n ->
									    let l = [1 .. n bitand 0xFF] in
									           elements_are_found           l
									        /\ non_elements_are_not_found m l
									        /\ True
									)

elements_are_found				:: [Int] -> Property
elements_are_found l			= name "elements are found"
										(ForEach l (\x -> let i = l??x in 0 <= i && i < length l && l!!i == x))

non_elements_are_not_found		:: Int [Int] -> Property
non_elements_are_not_found m l	= name "non-elements are not found"
										(not (isMember m l)) ==> l??m == -1
