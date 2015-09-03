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

-- sowing action
--  -------------------------------
-- | ( 4) ( 4) ( 4) ( 4) ( 4) ( 4) |
-- |                               |
-- | ( 4) ( 0) ( 4) ( 4) ( 4) ( 4) |
--         ->   +1   +1   +1   +1
--
sowing :: Int -> WholeBoard -> WholeBoard
sowing n nowboard = constructBoard n $ spreadSeeds $ distructBoard n nowboard

spreadSeeds :: [Int] -> [Int]
spreadSeeds [] = []
spreadSeeds (x:xs)
	| x <= 0 = xs
	| x <= (length xs) = map (\ n -> n + 1) (fst (splitAt x2int xs)) ++ ( snd (splitAt x2int xs))
	| otherwise = spreadSeeds $ (x - (length xs)): (map (\ n -> n + 1) xs)
	where
		x2int = (x ::Int)

--distructBoard :: (Integral b) => Int -> ((b,b,b,b,b,b), (b,b,b,b,b,b)) -> [b]
distructBoard :: Int -> WholeBoard -> [Int]
distructBoard a (myboard, opoboard) = myboard_tail ++ (taple2list opoboard) ++ myboard_head
	where
		myboard_head = take (a-1) $ taple2list myboard
		myboard_tail = snd $ splitAt (a-1) $ taple2list myboard

--constructBoard :: Integral b => Int -> [b] -> ((b,b,b,b,b,b), (b,b,b,b,b,b))
constructBoard :: Int -> [Int] -> WholeBoard
constructBoard a houselist = (list2taple (myhouses_head ++ (0:myhouses_tail)), list2taple opohouses)
	where
		opohouses = take 6 $ snd $ splitAt (6-a) houselist
		myhouses_tail = take (6-a) houselist
		myhouses_head = snd $ splitAt 6 $ snd $ splitAt (6-a) houselist
 
taple2list :: (b,b,b,b,b,b) -> [b]
taple2list (b1,b2,b3,b4,b5,b6) = [b1,b2,b3,b4,b5,b6]

list2taple :: [b] -> (b,b,b,b,b,b)
list2taple (a1:a2:a3:a4:a5:a6:xs) = (a1,a2,a3,a4,a5,a6)

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
