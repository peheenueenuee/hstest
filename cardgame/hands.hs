-- hands definition
--
module Hands
	( Hand
	, toHand, fromHand
	, PorkerHand(..)
	, porkerHand
	, straightHint, flushHint, nOfAKindHint
	, straightFlush
	, fourOfAKind
	, fullHouse
	, straight
	, flush
	, threeOfAKind
	, twoPair
	, onePair
	) where

import Cards
import Data.List
import Control.Monad

newtype Hand = Hand { fromHand :: [Card] } deriving (Show, Eq, Ord)

toHand :: [Card] -> Maybe Hand
toHand l =
	if length l == 5
		then Just $ Hand (sort l)
		else Nothing

porkerHand :: Hand -> (PorkerHand, Card)
porkerHand h@(Hand l) =  
	case foldl mplus Nothing $ fmap ($h) hands of
		Just pc -> pc
		Nothing -> (HighCard, last l)
	where
		hands :: [Hand -> Maybe (PorkerHand, Card)]
		hands = 
			[ straightFlush
			, fourOfAKind
			, fullHouse
			, straight
			, flush
			, threeOfAKind
			, twoPair
			, onePair
			]
		
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


onePair :: Hand -> Maybe (PorkerHand, Card)
onePair h = do
	cs <- nOfAKindHint 2 h
	return (OnePair, last $ concat cs)

twoPair :: Hand -> Maybe (PorkerHand, Card)
twoPair h = do
	cs <- nOfAKindHint 2 h
	if length cs == 2
		then Just (TwoPair, last $ concat cs)
		else Nothing

threeOfAKind :: Hand -> Maybe (PorkerHand, Card)
threeOfAKind h = do
	cs <- nOfAKindHint 3 h
	return (OnePair, last $ concat cs)

straight :: Hand -> Maybe (PorkerHand, Card)
straight h = do
	c <- straightHint h
	return (Straight, c)

flush :: Hand -> Maybe (PorkerHand, Card)
flush h = do
	c <- flushHint h
	return (Flush, c)

fullHouse :: Hand -> Maybe (PorkerHand, Card)
fullHouse h = do
	cs1 <- nOfAKindHint 3 h
	cs2 <- nOfAKindHint 2 h
	return (FullHouse, maximum $ concat cs1 ++ concat cs2)

fourOfAKind :: Hand -> Maybe (PorkerHand, Card)
fourOfAKind h = do
	cs <- nOfAKindHint 4 h
	return (OnePair, last $ concat cs)

straightFlush :: Hand -> Maybe (PorkerHand, Card)
straightFlush h = do
	c1 <- flushHint h
	c2 <- straightHint h
	return (StraightFlush, c2)

-- Hand decision
straightHint :: Hand -> Maybe Card
straightHint (Hand l) = 
	(judgeStraight . extract cardStrength $ l)
	`mplus`
	(judgeStraight . sort . extract cardNumber $ l)
	where
		isStraight :: [Int] -> Bool
		isStraight xs@(x:_) = xs == [x .. x+4]
		isStraight _ = False

		judgeStraight :: [(Int, Card)] -> Maybe Card
		judgeStraight l =
			if isStraight $ map fst l
			then Just . snd . last $ l
			else Nothing

extract :: (a -> b) -> [a] -> [(b, a)]
extract f cs = map (\c -> (f c, c)) cs

flushHint :: Hand -> Maybe Card
flushHint (Hand (x:xs)) = 
	if all ((cardSuit x ==).cardSuit) xs
	then Just (last xs)
	else Nothing

nOfAKindHint :: Int -> Hand -> Maybe [[Card]]
nOfAKindHint n (Hand h) = 
	if cards /= []
	then Just cards
	else Nothing
		where
		cards :: [[Card]]
		cards = filter ((== n).length) $
			groupBy (\x y -> (cardNumber x) == (cardNumber y)) h
