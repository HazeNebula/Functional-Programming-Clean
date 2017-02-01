implementation module RomanNumeral

import StdEnv
import StdDebug

//	You can use the below Start-functions to test your implementations:
Start						:: [Roman]
Start						= [fromInt 42, fromInt 999, fromInt 1024]

//Start						:: [Int]
//Start						= [toInt (Roman [X,L,I,I]), toInt (Roman [C,M,X,C,I,X]), toInt (Roman [C,D,X,L,I,V])]

:: RD						= M | D | C | L | X | V | I
:: Roman					= Roman [RD]

instance toInt RD where
	toInt M					= 1000
	toInt D					= 500
	toInt C					= 100
	toInt L					= 50
	toInt X					= 10
	toInt V					= 5
	toInt I					= 1

//	1.
instance toInt Roman where
	toInt _ = trace_n "instance toInt Roman not yet implemented" zero

//	2.
instance fromInt Roman where
	fromInt _ = trace_n "instance fromInt Roman not yet implemented" (Roman [])

