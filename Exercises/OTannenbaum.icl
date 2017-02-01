implementation module OTannenbaum

import StdEnv
import StdDebug

Start					= triangle 5
//Start					= christmastree 5

//////////////////////////////////////////////////
//
//	triangle
//
//////////////////////////////////////////////////

triangle	:: Int -> String
triangle n	=	triangle` n 1

triangle`				 ::	Int Int -> String
triangle` 0 _			 =	""
triangle` n numAsterisks =	space ( n - 1 ) +++
							asterisks numAsterisks +++
							"\n" +++
							triangle` ( n - 1 ) ( numAsterisks + 2 )

space	::	Int -> String
space 0 =	""
space n =	" " +++ space ( n - 1 )

asterisks	::	Int -> String
asterisks 0 =	""
asterisks n =	"*" +++ asterisks ( n - 1 )

//////////////////////////////////////////////////
//
//	christmastree
//
//////////////////////////////////////////////////

christmastree	:: Int -> String
christmastree n =	christmastree` n 1

christmastree`	   :: Int Int -> String
christmastree` 0 _ =	""
christmastree` n m =	genTriangle m ( n - 1 ) +++
											christmastree` ( n - 1 ) ( m + 1 )

genTriangle				  :: Int Int -> String
genTriangle n extraSpaces =	genTriangle` n 1 extraSpaces

genTriangle`							::	Int Int Int -> String
genTriangle` 0 _ _						=	""
genTriangle` n numAsterisks extraSpaces	=	space ( n - 1 + extraSpaces ) +++
											asterisks numAsterisks +++
											"\n" +++
											genTriangle` ( n - 1 ) ( numAsterisks + 2 ) extraSpaces