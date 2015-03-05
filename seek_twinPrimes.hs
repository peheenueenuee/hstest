
main :: IO()
main = do 
	let maxValue = 1000
	print $ [(a, a + 2) | a <- filter (\x ->((foldr (*) 1 [1,2..(x-1)] + 1) * 4 + x) `mod` (x^2 + x*2) == 0) [1..maxValue]]
