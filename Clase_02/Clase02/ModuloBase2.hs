module ModuloBase2(absoluto)
where
    
    absoluto :: Int -> Int
    absoluto x | x >= 0 = x
               | otherwise = negativo x
           
    negativo :: Int -> Int
    negativo x = -x 
