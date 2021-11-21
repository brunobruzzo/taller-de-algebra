-- Dados un numero real y un vector de R2 devolver el producto escalar a.x
prodEscalar :: Float -> (Float,Float) -> (Float,Float)
prodEscalar a (x,y) = (x*a , y*a)

-- Producto vectorial de dos vectores R3
prodVectorial :: (Float,Float,Float) -> (Float,Float,Float) -> (Float,Float,Float)
prodVectorial (x1,y1,z1) (x2,y2,z2) = (y1*z2 - y2*z1,z1*x2 - x1*z2,x1*y2 - y1*x2)

-- Ana y Beto juegan en equipo a embocar bolitas en un aro, por cada embocada suman 1 punto, ganan si suman entre los dos al menos 20 puntos.

juegoBolitas :: Int -> Int -> (Int,Bool)
juegoBolitas x y = (x + y, x + y >=20)

funcionFalopita :: Int -> Float
funcionFalopita 1.3 = 4


raiz :: Bool -> Bool
raiz x = sqrt(x)

