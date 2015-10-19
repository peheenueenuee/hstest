module Main where
import System.Random.Shuffle
import Cards
import Hands
import Data.List
import Control.Monad

main :: IO ()
main = do
	forM_ [1..500] $ \i -> do
		hand <- randomHand
		res <- return $ judgePorker hand
		putStrLn $ show hand ++ "->" ++ show res

type DiscardList = [Card]
type Deck = [Card]

drawHand :: Deck -> DiscardList -> Hand -> Maybe Deck
drawHand = undefined

randomHand :: IO (Maybe Hand)
randomHand = do
	shuffled <- shuffleM allCards
	return . toHand . take 5 $ shuffled

judgePorker :: Maybe Hand -> Maybe (PorkerHand, Card)
judgePorker h = do
	i <- h
	return $ porkerHand i

