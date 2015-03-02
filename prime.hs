main :: IO ()
main = print $ length $ filter isPrime [100000..1000000]

primes :: Integral a => [a]
primes = 2 : filter isPrime [3, 5..]

isPrime :: Integral a => a -> Bool
isPrime n = n > 1 && foldr (\p r -> p * p > n || (n `mod` p /= 0 && r)) True primes 
