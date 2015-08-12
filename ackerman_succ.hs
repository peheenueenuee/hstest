main :: IO()
main = do 
	print $ ackerman 3 11

ackerman :: (Enum a, Integral a) => a -> a -> a
ackerman 0 y = succ y
ackerman x 0 = ackerman (pred x) 1
ackerman x y = ackerman (pred x) (ackerman x (pred y))
