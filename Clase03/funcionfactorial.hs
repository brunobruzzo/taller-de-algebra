factorial :: Int -> Int
factorial n
           | n == 0 = 1
	   | n  > 0 = n * factorial (n-1)
           
superfactorial :: Integer -> Integer
superfactorial n
             | n == 0 = 1
             | n  > 0 = n * superfactorial (n-1)

esPar :: Int -> Bool
esPar n | n == 0  = True
        | n == 1 = False
        | otherwise = esPar (n-2) 
            



