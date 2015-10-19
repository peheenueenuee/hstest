--  cards difinition
--
module Cards
	( Suit(..)
	, Card
	, allCards
	, cardSuit
	, cardNumber
	, cardStrength 
	) where

data Suit = Spades | Hearts | Diamonds | Clubs
	deriving (Show, Read, Eq, Ord, Enum)

data Card = Card Int Suit
	deriving (Ord, Eq)

showCardNumber :: Int -> String
showCardNumber 14 = "A_"
showCardNumber 13 = "K_"
showCardNumber 12 = "Q_"
showCardNumber 11 = "J_"
showCardNumber 10 = "10"
showCardNumber n = (show n) ++ "_" 

instance Show Card where
	show (Card i Spades) = "S" ++ showCardNumber i 
	show (Card i Hearts) = "H" ++ showCardNumber i
	show (Card i Diamonds) = "D" ++ showCardNumber i
	show (Card i Clubs) = "C" ++ showCardNumber i

allCards :: [Card]
allCards = do
	suit <- [Spades ..]
	num <- [2..14]
	return $ Card num suit

cardSuit :: Card -> Suit
cardSuit (Card _ s) = s

cardNumber :: Card -> Int
cardNumber (Card 14 _) = 1
cardNumber (Card n _) = n

cardStrength :: Card -> Int
cardStrength (Card n _) = n
