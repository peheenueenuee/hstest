-- hands definition
--
module Hands
	( Hand
	, toHand, fromHand
	) where

import Cards
import Data.List

newtype Hand = Hand { fromHand :: [Card] } deriving (Show, Eq, Ord)

toHand :: [Card] -> Maybe Hand
toHand l =
	if length l == 5
		then Just $ Hand (sort l)
		else Nothing

porkerHand :: Hand -> (PorkerHand, Card)
porkerHand = undefined

data PorkerHand
	= HighCard
	| OnePair
	| TwoPair
	| ThreeOfAKind
	| Straight
	| Flush
	| FullHouse
	| FourOfAKind
	| StraightFlush
	deriving (Show, Read, Eq, Ord, Enum)

twoPair :: Hand -> Maybe PorkerHand
twoPair = undefined

-- Hand decision
straightHint :: Hand -> Maybe Card
straightHint = undefined

flushHint :: Hand -> Maybe Card
flushHint (Hand h) = 
	if all ((cardSuit ( head h ) ==).cardSuit) h
	then Just (last h)
	else Nothing

nOfAKindHint :: Hand -> Maybe [[Card]]
nOfAKindHint = undefined
