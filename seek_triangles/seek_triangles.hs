main :: IO()
main = do 
	print "Type n(0 <= n <= 10000)."
	inputValue <- getLine
	let intValue = read inputValue :: Int
	print $ length $ filter safeTriangleFilter (makeTriples intValue)

makeTriples :: (Enum a, Num a, Ord a) => a -> [(a, a, a)]
makeTriples n
	| n > 0 = [(a, b, c) | c <- [1..n], a <- [1..c], b <- [1..a]]
	| otherwise = error "too small"

safeTriangleFilter :: (Num a, Ord a) => (a, a, a) -> Bool
safeTriangleFilter (a, b, c)
	| a + b <= c = False
	| b + c <= a = False
	| c + a <= b = False
	| otherwise = True

