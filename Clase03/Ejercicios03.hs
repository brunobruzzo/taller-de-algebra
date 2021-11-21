main = undefined
-- Escribir una funcion para determinar sinumero natural es multiplo de 3. NO esta permitido usar mod ni div.

multiplo3 :: Int -> Bool
multiplo3 n | n == 3 = True
            | n <  3 = False
            | n > 3 = multiplo3 (n-3)

-- dado n ∈ N sume los primeros n numeros impares. Ej: sumaImpares 3 -> 1+3+5 -> 9.

sumaImpares :: Int -> Int
sumaImpares n | n == 1 = 1
              | n >  1 = (2*n - 1) + sumaImpares (n-1)
              | n <  1 = undefined

-- dado n ∈ N calcula n!!=n(n−2)(n−4)···

medioFact :: Integer -> Integer
medioFact n | n <= 1 = 1
            | n >  1 = n * medioFact (n-2)

-- determine la suma de dıgitos de un numero positivo.
sumaDigitos :: Int -> Int
sumaDigitos n | n == 0 = 0
              | n > 0 = (mod n 10) + sumaDigitos (div n 10)
              | n < 0 = undefined

digitosIguales :: Int ->  Bool
digitosIguales n =  ((div n 10) == 0) ||  ((mod n 10) == (digitoDecenas n) &&  digitosIguales (div n 10))
digitoDecenas n = div (mod n 100) 10 
