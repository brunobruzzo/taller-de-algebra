module ModuloBase
where

    maximo :: Int -> Int -> Int
    maximo x y | x >= y = x
               | otherwise = y

    divide :: Int -> Int -> Bool
    divide x y = mod y x == 0
               
    -- Mas y mas identificadores
