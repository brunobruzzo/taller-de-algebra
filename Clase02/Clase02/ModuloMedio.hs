module ModuloMedio
where
    import ModuloBase
    import ModuloBase2(absoluto)
    
    maximo3 :: Int -> Int -> Int -> Int
    maximo3 x y z = maximo (maximo x y) z

    maximoAbsoluto :: Int -> Int -> Int
    maximoAbsoluto x y = maximo (abs x) (absoluto y)
