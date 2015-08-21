-- 
-- mancala.hs
--
--

main :: IO()
main = do
	displayBoard smpwhole
	
data Player = Me | Opo
type OnesBoard = (Int, Int, Int, Int, Int, Int) 
type WholeBoard = (OnesBoard, OnesBoard) 

smpboard = (4, 12, 3, 5, 0, 4)
smpwhole = (smpboard, smpboard)

sowing :: Integral a => WholeBoard -> a -> WholeBoard
sowing (myboard, opoboard) n
	| n <= 0 = (myboard, opoboard)
	| n > 6 = (myboard, opoboard)
	| otherwise = (myboard, opoboard)

board2List :: OnesBoard -> [Int]
board2List (a1, a2, a3, a4, a5, a6) = [a1, a2, a3, a4, a5, a6]

selectHouse :: OnesBoard -> OnesBoard -> Int -> [Int]
selectHouse myboard opoboard n
	| n <= 0 = []
	| n > 6 = []
	| otherwise =  snd ( splitAt n (board2List myboard)) ++ (board2List opoboard) ++ take (n-1) (fst ( splitAt n (board2List myboard))) 

-- game board
--  -------------------------------
-- | ( 4) ( 4) ( 4) ( 4) ( 4) ( 4) |
-- |                               |
-- | ( 4) ( 4) ( 4) ( 4) ( 4) ( 4) |
--  -------------------------------
displayBoard :: WholeBoard -> IO()
displayBoard (myboard, opoboard) = do
	putStrLn "  ------------------------------- "
	putStrLn $ " |" ++ (createMyBoard Opo opoboard) ++ " |" 
	putStrLn " |                               |"
	putStrLn $ " |" ++ (createMyBoard Me myboard) ++ " |" 
	putStrLn "  ------------------------------- "

createMyBoard :: Player -> OnesBoard -> [Char]
createMyBoard Me (a1,a2,a3,a4,a5,a6) = concat $ map createHouse [a1,a2,a3,a4,a5,a6] 
createMyBoard Opo (a1,a2,a3,a4,a5,a6) = concat $ map createHouse $ reverse [a1,a2,a3,a4,a5,a6] 

createHouse :: (Integral a, Show a) => a -> [Char]
createHouse a
	| a <= 0 = " ( 0)"
	| a < 10 = " ( " ++ (show a) ++ ")"
	| a < 100 = " (" ++ (show a) ++ ")"
	| otherwise = " (99)"
