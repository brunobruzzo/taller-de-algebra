f x y = x * x + y * y

g x y z = x + y + z * z

f1 n | n >= 3 = 5 -- La barra | se llama guarda

f2 n | n >= 3 = 5
     | n <= 1 = 8

f3 n | n >= 3 = 5
     | n == 2 = undefined
     | otherwise = 8

f4 n | n >= 3 = 5
     | n <= 9 = 7

f5 n | n <= 9 = 7
     | n >= 3 = 5

-- Pattern Matching (dentro del dom, algunos elem cumplen un patron) P ej 0 = 1

h 0 = 1
h n = 0

-- tambien se puede hacer de esta otra manera.

i n | n == 0 = 1
    | n /= 0 = 0

-- Pattern Matching de funcion signo

signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1

signoPm 0 = 0
signoPm n | n > 0 = 1
          | otherwise = (-1)

-- Implementar funcion Cantidad de Soluciones

cantidadDeSoluciones b c | b^2 - 4*c > 0 = 2
                         | b^2 - 4*c == 0 = 1
                         | otherwise = 0

cantidadDeSolucionesPm b c | d > 0 = 2
                           | d == 0 = 1
                           | otherwise = 0 
                           where d = b^2 - 4*c -- crea una variable para no repetir en cada evaluacion la funcion.

maximo :: Int -> Int -> Int
maximo x y | x >= y = x
           | otherwise = y


maximoRac :: Float -> Float -> Float 
maximoRac x y | x >= y = x
              | otherwise = y

esMayorA9 :: Int -> Bool 
esMayorA9 n | n > 9 = True
            | otherwise = False

esPar :: Int -> Bool
esPar n | mod n 2 == 0 = True
        | otherwise = False

esPar2 :: Int -> Bool
esPar2 n = mod n 2 == 0

esImpar :: Int -> Bool 
esImpar n = not (esPar n)

funcionRara :: Float -> Float -> Bool -> Bool 
funcionRara x y z = (x >= y) || z
