-- 
-- mancala.hs
--
--

main :: IO()
main = do
	displayBoard smpboard smpboard
	
data Player = Me | Opo
type OnesBoard = (Int, Int, Int, Int, Int, Int) 
smpboard = (4, 12, 3, 5, 0, 4)

-- game board
--  -------------------------------
-- | ( 4) ( 4) ( 4) ( 4) ( 4) ( 4) |
-- |                               |
-- | ( 4) ( 4) ( 4) ( 4) ( 4) ( 4) |
--  -------------------------------
displayBoard :: OnesBoard -> OnesBoard -> IO()
displayBoard myboard opoboard = do
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
