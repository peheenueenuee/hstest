module Main where
import System.Random.Shuffle
import Cards
import Hands
import Data.List
import Data.Char
import Control.Monad
import Control.Applicative
import Safe

main :: IO ()
main = do
	putStrLn "---------------------"
	putStrLn "--- Simple Porker ---"
	putStrLn "---------------------"
	deck <- shuffleM allCards
	case getHand deck of
		Nothing -> error "Unspecified ERROR"
		Just (hand, deck_) -> playPorker hand deck_
-- 	ynQuestion "--- Play again?" main 
	(putStrLn "--- Bye.")

playPorker :: Hand -> Deck -> IO ()
playPorker h0 d0 = putStrLn "(porker)"

handsTest :: IO ()
handsTest = do
	forM_ [1..500] $ \i -> do
		hand <- randomHand
		res <- return $ judgePorker hand
		putStrLn $ show hand ++ "->" ++ show res

type DiscardList = [Card]
type Deck = [Card]

getHand :: Deck -> Maybe (Hand, Deck)
getHand deck = do
	hand <- toHand.take 5 $ deck
	return (hand, drop 5 deck)

getDiscardList :: Hand -> IO (Maybe DiscardList)
getDiscardList h = do -- IO monad
	input <- getLine
	return $ do -- Maybe monad
		intList <- toIntList input
		res <- selectByIndice (fromHand h) intList
		return res

toIntList :: String -> Maybe [Int]
toIntList str = if (and (map isDigit str))
	then Just (reads str)
	else Nothing 
	where
		reads :: String -> [Int]
		reads = map $ read. (:[])

selectByIndice :: [a] -> [Int] -> Maybe [a]
selectByIndice l = sequence . map ((atMay l) . (subtract 1))

drawHand :: Deck -> DiscardList -> Hand -> Maybe (Hand, Deck)
drawHand deck dis h = let
	nl = filter (flip notElem dis) (fromHand h)
	nr = drop (5 - (length nl)) deck
	in (,) <$> toHand (take 5 $ nl ++ deck) <*> return nr

randomHand :: IO (Maybe Hand)
randomHand = do
	shuffled <- shuffleM allCards
	return . toHand . take 5 $ shuffled

judgePorker :: Maybe Hand -> Maybe (PorkerHand, Card)
judgePorker h = do
	i <- h
	return $ porkerHand i

