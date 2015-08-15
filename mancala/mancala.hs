-- 
-- mancala.hs
--
--

main :: IO()
main = do
	displayBoard
	
-- game board
--  -------------------------------
-- | ( 4) ( 4) ( 4) ( 4) ( 4) ( 4) |
-- |                               |
-- | ( 4) ( 4) ( 4) ( 4) ( 4) ( 4) |
--  -------------------------------
displayBoard :: IO()
displayBoard = do
	putStrLn "  ------------------------------- "
	putStrLn " | ( 4) ( 4) ( 4) ( 4) ( 4) ( 4) |"
	putStrLn " |                               |"
	putStrLn " | ( 4) ( 4) ( 4) ( 4) ( 4) ( 4) |"
	putStrLn "  ------------------------------- "

createMyBoard :: Integral a => [a] -> (a,a,a,a,a,a)
createMyBoard [] = (0,0,0,0,0,0) 
