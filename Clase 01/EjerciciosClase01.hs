
-- Calcula el valor absoluto de un numero entero
absoluto :: Int -> Int
absoluto x | x < 0 = x * (-1)
           | otherwise = x

-- Devuelve el maximo entre el valor absoluto de dos numeros enteros
maximoabsoluto :: Int -> Int -> Int
maximoabsoluto x y | absoluto x > absoluto y = absoluto x
                   | otherwise = absoluto y

-- Devuelve el maximo entre tres numeros enteros
max2 :: Int -> Int -> Int
max2 x y | x >= y = x
         | otherwise = y

max3 :: Int -> Int -> Int -> Int
max3 x y z | max2 x y >= z = max2 x y
           | otherwise = z

-- Dados dos numeros racionales, decide si alguno de los dos es igual a 0 (Sin pattern matching)
algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y | x == 0 = True
              | y == 0 = True
              | otherwise = False

-- Same con Pattern Matching
algunoEs0PM :: Float -> Float -> Bool
algunoEs0PM x y = x * y == 0

-- Dados dos numeros racionales, decidir si ambos son iguales a 0. (Con pattern mathing)
ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y = (x == 0 && y == 0)

-- Dados dos numeros naturales, decidir si el primero es multiplo del segundo
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = (mod x y == 0)

-- Dado un numero natural, extrae su digito de las unidades
digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

-- Dado un numero natural, extrae su digito de las decenas
digitoDecenas :: Int -> Int
digitoDecenas x = div (mod x 100) 10 

-- Voy a natacion los Lun y Mie de Septiembre. No voy el 29. Voy a natacion el dia x?
natacion :: Int -> Bool
natacion 29 = False
natacion x = mod x 7 == 1 || mod x 7 == 6

-- Dados dos numeros naturales x y, devuelve el n-esimo digito de x.
nesimoDig :: Int -> Int -> Int
nesimoDig x n = div ( mod x (10^n) ) (10^(n-1))

-- Resolucion by Chapa
digitoN :: Int -> Int -> Int
digitoN x n = mod (div x (10^(n-1))) 10
