import Control.Monad
vava = [(1,2),(3,4),(5,6)]
pop = [1,2,3]
inputValue = 150

deck = [1..13]

endressSuck :: Char -> [Char]
endressSuck '\128' = []
endressSuck x = succ x : endressSuck (succ x)

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
raiseOfSwamp oop@(x:_) = oop ++ [x] 

funcConnect::Fractional a => a -> a
funcConnect x = x * 2.02

sort_A_Bubble::Ord a => [a] -> a 
sort_A_Bubble (x:xs) = max x (head xs)

first_Dim_Func::Num a => a -> a -> a -> a
first_Dim_Func a b x = a*x + b
