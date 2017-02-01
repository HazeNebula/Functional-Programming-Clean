definition module Cards

import Card

carddeck      :: [Card]				// complete set of cards
sort_by_value :: [Card] -> [Card]	// sort a deck of cards, first by value, then by suit
sort_by_suit  :: [Card] -> [Card]	// sort a deck of cards, first by suit, then by value
