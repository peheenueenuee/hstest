-- 
-- mancala.hs
--
--

main :: IO()
main = do
	displayBoard
	
myboard = (4, 12, 3, 5, 0, 4)

-- game board
--  -------------------------------
-- | ( 4) ( 4) ( 4) ( 4) ( 4) ( 4) |
-- |                               |
-- | ( 4) ( 4) ( 4) ( 4) ( 4) ( 4) |
--  -------------------------------
displayBoard :: IO()
displayBoard = do
	putStrLn "  ------------------------------- "
	putStrLn $ " |" ++ (createMyBoard myboard) ++ " |" 
	putStrLn " |                               |"
	putStrLn $ " |" ++ (createMyBoard myboard) ++ " |" 
	putStrLn "  ------------------------------- "

createMyBoard :: (Integral a, Show a) => (a,a,a,a,a,a) -> [Char]
createMyBoard (a1,a2,a3,a4,a5,a6) = concat $ map createHouse [a1,a2,a3,a4,a5,a6] 

createHouse :: (Integral a, Show a) => a -> [Char]
createHouse a
	| a <= 0 = " ( 0)"
	| a < 10 = " ( " ++ (show a) ++ ")"
	| a < 100 = " (" ++ (show a) ++ ")"
	| otherwise = " (99)"
