module Main where
import System.Random.Shuffle
import Cards
import Hands
import Data.List

main :: IO ()
main = do
	shuffled <- shuffleM allCards
	print . sort . take 5 $ shuffled

