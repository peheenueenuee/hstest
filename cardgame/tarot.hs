-- Tarot cards difinition
--
module Tarots
	( Suit(..)
	, Card
	, allCards
	, cardSuit
	, cardNumber
	) where

data Suit = Epee | Coupe | Denier | Baton
	deriving (Show, Read, Eq, Ord, Enum)

data Card = Card Int Suit
	deriving (Ord, Eq)

showCardNumber :: Int -> String
showCardNumber 14 = "Roi"
showCardNumber 13 = "Rei"
showCardNumber 12 = "Cav"
showCardNumber 11 = "Val"
showCardNumber n = (show n) 

instance Show Card where
	show (Card i Epee) = showCardNumber i ++ "Ep_"  
	show (Card i Coupe) = showCardNumber i ++  "Co_" 
	show (Card i Denier) = showCardNumber i ++  "De_"
	show (Card i Baton) = showCardNumber i ++  "Ba_"

allCards :: [Card]
allCards = do
	suit <- [Epee ..]
	num <- [1..14]
	return $ Card num suit

cardSuit :: Card -> Suit
cardSuit (Card _ s) = s

cardNumber :: Card -> Int
cardNumber (Card n _) = n

