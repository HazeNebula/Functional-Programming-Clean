//Coded by Niels van Nistelrooij (s4713648) and Jasper van den Bogart (s4781686)

implementation module Cards

import StdEnv
import StdDebug
import Card

carddeck								:: [Card]
carddeck						= carddeck` Two Heart
	where
		carddeck`						:: Value Suit -> [Card]
		carddeck` Ace Club		= [{suit = Club, value = Ace}]
		carddeck` Ace suit		= [{suit = suit, value = Ace}] ++ carddeck` Two (nextSuit suit)
		carddeck` value suit	= [{suit = suit, value = value}] ++ carddeck` (nextValue value) suit

nextValue								:: Value -> Value
nextValue Two	= Three
nextValue Three	= Four
nextValue Four	= Five
nextValue Five	= Six
nextValue Six	= Seven
nextValue Seven	= Eight
nextValue Eight	= Nine
nextValue Nine	= Ten
nextValue Ten	= Jack
nextValue Jack	= Queen
nextValue Queen	= King
nextValue King  = Ace
nextValue Ace	= Two

nextSuit								:: Suit -> Suit
nextSuit Heart		= Diamond
nextSuit Diamond	= Spade
nextSuit Spade		= Club
nextSuit Club		= Heart

sort_by_value							:: [Card] -> [Card]
sort_by_value cards																				= sort_by_value` (sortByValue cards) (length cards - 1)
	where
		sort_by_value`					:: [Card] Int -> [Card]
		sort_by_value` cards 0																	= sortBySuit cards
		sort_by_value` cards int
		| (cards !! (length cards - 1 - int)).value == (cards !! (length cards - int)).value	= sort_by_value` cards (int - 1)
		| otherwise																				= sortBySuit (cards % (0,length cards - 1 - int))
																								++ sort_by_value` (cards % (length cards - int, length cards - 1)) (length (cards % (length cards - int, length cards - 1)) - 1)
		
sort_by_suit							:: [Card] -> [Card]
sort_by_suit cards																			= sort_by_suit` (sortBySuit cards) (length cards - 1)
	where
		sort_by_suit`					:: [Card] Int -> [Card]
		sort_by_suit` cards 0 																= sortByValue cards
		sort_by_suit` cards int
		| (cards !! (length cards - 1 - int)).suit == (cards !! (length cards - int)).suit	= sort_by_suit` cards (int - 1)
		| otherwise																			= sortByValue (cards % (0, length cards - 1 - int)) 
																							++ sort_by_suit` (cards % (length cards - int, length cards - 1)) (length (cards % (length cards - int, length cards - 1)) - 1)


sortBySuit								:: [Card] -> [Card]
sortBySuit cards											= sortBySuit` cards (length cards - 1) Heart
	where
		sortBySuit`						:: [Card] Int Suit -> [Card]
		sortBySuit` cards 0 Club
		| (cards !! (length cards - 1)).suit == Club		= [(cards !! (length cards - 1))]
		| otherwise											= []
		sortBySuit` cards 0 suit
		| (cards !! (length cards - 1)).suit == suit		= [cards !! (length cards - 1)] ++ sortBySuit` cards (length cards - 1) (nextSuit suit)
		| otherwise											= sortBySuit` cards (length cards  - 1) (nextSuit suit)
		sortBySuit` cards int suit
		| (cards !! (length cards - 1 - int)).suit == suit	= [(cards !! (length cards - 1 - int))] ++ (sortBySuit` cards (int - 1) suit)
		| otherwise											= sortBySuit` cards (int - 1) suit
	
sortByValue								:: [Card] -> [Card]
sortByValue cards												= sortByValue` cards (length cards - 1) Two
	where
		sortByValue`					:: [Card] Int Value -> [Card]
		sortByValue` cards 0 Ace
		| (cards !! (length cards - 1)).value == Ace			= [(cards !! (length cards - 1))]
		| otherwise												= []
		sortByValue` cards 0 value
		| (cards !! (length cards - 1)).value == value			= [(cards !! (length cards - 1))] ++ sortByValue` cards (length cards - 1) (nextValue value)
		| otherwise												= sortByValue` cards (length cards - 1) (nextValue value)
		sortByValue` cards int value
		| (cards !! (length cards - 1 - int)).value == value	= [(cards !! (length cards - 1 - int))] ++ sortByValue` cards (int - 1) value
		| otherwise												= sortByValue` cards (int - 1) value


//Start 	= sort_by_suit (carddeck ++ [fromString "Five of Diamonds", fromString "Ace of Hearts"])
Start	= sort_by_value (carddeck ++ [fromString "Five of Diamonds", fromString "Ace of Hearts"])

