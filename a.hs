import Control.Monad
vava = [(1,2),(3,4),(5,6)]
pop = [1,2,3]
inputValue = 150

smphouses = [1,2,3,4,5,6,1,2,3,4,5]
smphouses3 = [4,5,6,1,2,3,4,5,6,1,2]
smphouses4 = [5,6,1,2,3,4,5,6,1,2,3]
smpboard = ((1,2,3,4,5,6), (2,2,3,4,5,6))
smpmyboard = (1,2,3,4,5,6)
newboard = ((4,4,4,4,4,4), (4,4,4,4,4,4))


sowing :: (Integral b) => Int -> ((b,b,b,b,b,b), (b,b,b,b,b,b)) -> ((b,b,b,b,b,b), (b,b,b,b,b,b))
sowing n nowboard = constructBoard n $ spreadSeeds $ distructBoard n nowboard

switchSide :: (Integral b) => ((b,b,b,b,b,b), (b,b,b,b,b,b)) -> ((b,b,b,b,b,b), (b,b,b,b,b,b))
switchSide (myBoard, opoBoard) = (opoBoard, myBoard)

spreadSeeds :: Integral a => [a] -> [a]
spreadSeeds [] = []
spreadSeeds (x:xs)
	| x <= 0 = xs
	| x <= fromIntegral(length xs) = map (\ n -> n + 1) (fst (splitAt x2int xs)) ++ ( snd (splitAt x2int xs))
	| otherwise = spreadSeeds $ (x - fromIntegral(length xs)): (map (\ n -> n + 1) xs)
	where
		x2int = (fromIntegral x ::Int)

distructBoard :: (Integral b) => Int -> ((b,b,b,b,b,b), (b,b,b,b,b,b)) -> [b]
distructBoard a (myboard, opoboard) = myboard_tail ++ (taple2list opoboard) ++ myboard_head
	where
		myboard_head = take (a-1) $ taple2list myboard
		myboard_tail = snd $ splitAt (a-1) $ taple2list myboard

taple2list :: (b,b,b,b,b,b) -> [b]
taple2list (b1,b2,b3,b4,b5,b6) = [b1,b2,b3,b4,b5,b6]

list2taple :: [b] -> (b,b,b,b,b,b)
list2taple (a1:a2:a3:a4:a5:a6:xs) = (a1,a2,a3,a4,a5,a6)

constructBoard :: Integral b => Int -> [b] -> ((b,b,b,b,b,b), (b,b,b,b,b,b))
constructBoard a houselist = (list2taple (myhouses_head ++ (0:myhouses_tail)), list2taple opohouses)
	where
		opohouses = take 6 $ snd $ splitAt (6-a) houselist
		myhouses_tail = take (6-a) houselist
		myhouses_head = snd $ splitAt 6 $ snd $ splitAt (6-a) houselist
 

goisi :: Integral a => a -> [(a, a, a)]
goisi 0 = []
goisi univ = [(a, b, c) | a <- [1..univ], b <- [1..((univ + 1) `div` (a + 1))], c <- [a]]

triangles = [(a, b, c) | c <- [1..inputValue], a <- [1..c], b <- [1..a]]

pow :: (Enum a, Num a, Ord a) => a -> a
pow n
	| n <= 0 = 1
	| otherwise = foldr (*) 1 [1..n]

assortTriples :: (Enum a, Num a, Ord a) => a -> [(a, a, a)]
assortTriples n
	| n > 0 = [(a, b, c) | c <- [2..n], a <- [2..c], b <- [2..a]]
	| otherwise = error "too small"

compositeTriples :: Num a => [(a, a, a)] -> [a]
compositeTriples [] = []
compositeTriples ((a, b, c) : xs) = (a * b * c) : (compositeTriples xs) 

safeTriangleFilter :: (Num a, Ord a) => (a, a, a) -> Bool
safeTriangleFilter (a, b, c)
	| a + b <= c = False
	| b + c <= a = False
	| c + a <= b = False
	| otherwise = True

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (filter (\n -> n <= x) xs) ++ [x] ++ quickSort (filter (\n -> n >= x) xs)

duplicateFilter :: Eq a => [a] -> [a]
duplicateFilter [] = []
duplicateFilter (x:xs) = x:(filter (\n -> n /= x) (duplicateFilter xs))

eldrazi::Int -> Int
eldrazi 1 = 1
eldrazi n 
	| n >= 111 = 9999
	| otherwise = 8888

raiseOfSwamp::[a] -> [a]
raiseOfSwamp [] = []
raiseOfSwamp oop@(_:xs) = oop ++ xs

funcConnect::Fractional a => a -> a
funcConnect x = x * 2.02

sort_A_Bubble::Ord a => [a] -> a 
sort_A_Bubble (x:xs) = max x (head xs)

first_Dim_Func::Num a => a -> a -> a -> a
first_Dim_Func a b x = a*x + b
