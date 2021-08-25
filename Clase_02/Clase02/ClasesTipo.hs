triple :: (Num t) => t -> t
triple x = 3*x
  
maximo :: (Ord t) => t -> t -> t
maximo x y | x >= y = x
           | otherwise = y
             
distintos :: (Eq t) => t -> t -> Bool
distintos x y = x /= y
  
-- |Cantidad de raices de x^2 + bx + c
cantidadDeSoluciones :: (Num t, Ord t) => t -> t -> Int
cantidadDeSoluciones b c | d > 0 = 2
                         | d == 0 = 1
                         | otherwise = 0
    where d = b^2 - 4*c
  
pepe :: (Floating t, Eq t, Num u, Eq u) => t -> t -> u -> Bool
pepe x y z = sqrt (x + y) == x && 3*z == 0
