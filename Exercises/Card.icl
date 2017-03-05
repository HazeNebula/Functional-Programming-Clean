implementation module Card

import StdEnv
import StdDebug

Start = testFromString( fromString "Five of Diamonds")

testFromString				:: String -> Card
testFromString string = fromString string

//	1.
// Define Card, Suit, Value here:

:: Card = {suit :: Suit, value :: Value}
:: Suit = Heart | Diamond | Spade | Club
:: Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
		 | Jack | Queen | King | Ace
		 

		 
//	2.
instance == Card where
	== card0 card1	= card0.suit == card1.suit && card0.value == card1.value
	
instance == Suit where
	== Heart Heart		= True
	== Diamond Diamond	= True
	== Spade Spade		= True
	== Club Club		= True
	== _ _				= False
	
instance == Value where
	== Two Two		= True
	== Three Three	= True
	== Four Four	= True
	== Five Five	= True
	== Six Six		= True
	== Seven Seven	= True
	== Eight Eight	= True
	== Nine Nine	= True
	== Ten Ten		= True
	== Jack Jack	= True
	== Queen Queen	= True
	== King King	= True
	== Ace Ace		= True
	== _ _			= False

//	3.
instance toString Card where
	toString card	= toString card.value +++ " of " +++ toString card.suit +++ "s"

instance toString Suit where
	toString Heart		= "Heart"
	toString Diamond	= "Diamond"
	toString Spade		= "Spade"
	toString Club		= "Club"
	
instance toString Value where
	toString Two	= "Two"
	toString Three	= "Three"
	toString Four	= "Four"
	toString Five	= "Five"
	toString Six	= "Six"
	toString Seven	= "Seven"
	toString Eight	= "Eight"
	toString Nine	= "Nine"
	toString Ten	= "Ten"
	toString Jack	= "Jack"
	toString Queen	= "Queen"
	toString King	= "King"
	toString Ace	= "Ace"

instance fromString Card where
	fromString string = {value = fromString (string % (0,1)), suit = fromChar (string.[size string - 2])}
	
instance fromChar Suit where
	fromChar 't'	= Heart
	fromChar 'd'	= Diamond
	fromChar 'e'	= Spade
	fromChar 'b'	= Club
	
instance fromString Value where
	fromString "Tw"	= Two
	fromString "Th"	= Three
	fromString "Fo"	= Four
	fromString "Fi"	= Five
	fromString "Si"	= Six
	fromString "Se"	= Seven
	fromString "Ei"	= Eight
	fromString "Ni"	= Nine
	fromString "Te"	= Ten
	fromString "Ja"	= Jack
	fromString "Qu"	= Queen
	fromString "Ki"	= King
	fromString "Ac"	= Ace

