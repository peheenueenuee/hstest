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

