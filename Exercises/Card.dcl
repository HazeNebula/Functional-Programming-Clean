definition module Card

import StdEnv

// Copy your type definitions of Card, Suit, Value here to make them available for other modules:
:: Card = {suit :: Suit, value :: Value}
:: Suit = Heart | Diamond | Spade | Club
:: Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
		 | Jack | Queen | King | Ace

instance ==         Card
instance == 		Suit
instance ==			Value

instance toString   Card

instance fromString Card

