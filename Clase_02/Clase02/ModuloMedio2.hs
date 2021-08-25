module ModuloMedio2
where
    import ModuloBase
    import ModuloBase2
    import ModuloBase3
    
    maximo3 :: Int -> Int -> Int -> Int
    maximo3 x y z = ModuloBase.maximo (ModuloBase.maximo x y) z

    maximoCuadrado :: Float -> Float -> Float
    maximoCuadrado x y = ModuloBase3.maximo (x^2) (y^2)
