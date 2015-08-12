main :: IO()
main = do 
	print $ ackerman 3 11

ackerman :: (Integral a) => a -> a -> a
ackerman 0 y = y + 1
ackerman x 0 = ackerman (x - 1) 1
ackerman x y = ackerman (x - 1) (ackerman x (y - 1))
