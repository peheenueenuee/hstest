module Main where
import System.Random.Shuffle
import Cards
import Hands
import Data.List
import Control.Monad
import Control.Applicative

main :: IO ()
main = do
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
getDiscardList h = do
	input <- getLine
	return $ Just $ selectByIndice (fromHand h) (toIntList input)

toIntList :: String -> Maybe [Int]
toIntList = map $ read. (:[])

selectByIndice :: [a] -> [Int] -> Maybe [a]
selectByIndice l = map ((l!!) . (subtract 1))

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

