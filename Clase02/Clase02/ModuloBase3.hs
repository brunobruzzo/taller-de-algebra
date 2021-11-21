module ModuloBase3
where
    
    maximo :: Float -> Float -> Float
    maximo x y | x >= y = x
               | otherwise = y
               
    usoWhere :: Int -> Int
    usoWhere x = maximo + x
        where maximo = 5
