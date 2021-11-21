main = undefined

-- k es divisor de n
divisor :: Int -> Int -> Bool
divisor n k = mod n k == 0 

--sonCoprimos :: Int -> Int -> Bool
--sonCoprimos a b | mod a divisor == 0 && mod b divisor == 0 
--                | otherwise = False

esMenor :: Int -> Int -> Int
esMenor a b | a < b = a
            | otherwise = b

esMayor :: Int -> Int -> Int
esMayor a b | a > b = a
            | otherwise = b

sonCoprimos :: Int -> Int -> Bool
sonCoprimos a b | mod a (esMenor a b) == 0 && mod b (esMenor a b) == 0 = True
--                | otherwise = sonCoprimos esMayor (esMenor -1)
                  | otherwise = mod a ((esMenor a b)-1) == 0  && mod b ((esMenor a b) -1) == 0


sonCoprimos' :: Int -> Int -> Bool
sonCoprimos' a b | esMenor a b == 1 = True
                 | mod (esMayor a b) (esMenor a b) == 0 = False
                 | otherwise = sonCoprimos' (esMayor a b) ((esMenor a b)-1)

                 
