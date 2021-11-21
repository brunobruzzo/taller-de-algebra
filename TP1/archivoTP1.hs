esSumaDeDosCubos :: Integer -> Bool
esSumaDeDosCubos n | sumaCubos n (parteEnteraDeRaiz3 n) 1 > 0 = True
                   | otherwise = False
                   
descomposicionCubos :: Integer -> (Integer,Integer)
descomposicionCubos n = sumaCubosEj2 n (parteEnteraDeRaiz3 n) 1

cantidadDeFormas :: Integer -> Integer
cantidadDeFormas n = div (sumaCubos n (parteEnteraDeRaiz3 n) 1) 2

-- cantidadDeFormasAux n = sumaCubosEj2 n (parteEnteraDeRaiz3 n) 1 != (0, 0)
--                          yo voy a recibir de sumacubosej2 una tupla de la forma (segundoSu, primerSu)
--                         si la tupla es no esta vacia, quiero sumar uno, y volver a llamar a sumacubose
-- especialDesde :: Integer -> Integer

-- especialNumero :: Integer -> Integer

-- esMuyEspecial :: Integer -> Bool

esUnCubo :: Integer -> Bool
esUnCubo x = (round (fromIntegral x**(1/3)))^3 == x


-- A Partir de aca son nuestras func
-- parteEnteraDeRaiz3 :: Float -> Integer
parteEnteraDeRaiz3 :: Integer -> Integer
parteEnteraDeRaiz3 n = floor (fromIntegral(n) ** (1/3))

sumaCubos :: Integer -> Integer -> Integer -> Integer
sumaCubos numOriginal primerSumando segundoSumando | segundoSumando^3 > numOriginal = 0
                                                   | suma == numOriginal && primerSumando == segundoSumando = 2
                                                   | suma == numOriginal = 1 + sumaCubos numOriginal (primerSumando - 1) segundoSumando
                                                   | suma > numOriginal = sumaCubos numOriginal (primerSumando - 1) segundoSumando
                                                   | suma < numOriginal = sumaCubos numOriginal primerSumando (segundoSumando + 1)
  where suma = primerSumando^3 + segundoSumando^3

sumaCubosEj2 :: Integer -> Integer -> Integer -> (Integer, Integer)
sumaCubosEj2 numOriginal primerSumando segundoSumando | segundoSumando^3 > numOriginal = (0,0)
                                                      | suma == numOriginal = (segundoSumando, primerSumando)
                                                      | suma > numOriginal = sumaCubosEj2 numOriginal (primerSumando - 1) segundoSumando
                                                      | suma < numOriginal = sumaCubosEj2 numOriginal primerSumando (segundoSumando + 1)
  where suma = primerSumando^3 + segundoSumando^3


generadorEspeciales :: Integer -> Integer -> Integer-> Integer
generadorEspeciales a b n | a == b = generadorEspeciales a (b+1) n
                          | (cantidadDeFormas(a^3 + b^3) > 1) && (a^3 + b^3 >= n) = a^3 + b^3
                          | (cantidadDeFormas(a^3 + b^3) > 1) && (a^3 + b^3 < n) = generadorEspeciales (a+1) 1 n
                          | otherwise = generadorEspeciales a (b+1) n

-- Recursion de numeros especiales. Dame el proximo especial mayor a n.
especialDesde :: Integer -> Integer
especialDesde n | cantidadDeFormas n > 1 = n
                | otherwise = especialDesde (n+1)

especialNumero :: Integer -> Integer
especialNumero n = especialNumeroAux n 0 1

especialNumeroAux :: Integer  -> Integer -> Integer -> Integer
especialNumeroAux orden ind n | orden == ind = (n-1)
                              | cantidadDeFormas n > 1 = especialNumeroAux orden (ind+1) (n+1)
                              | otherwise = especialNumeroAux orden ind (n+1)

esEspecial :: Integer -> Bool
esEspecial n = cantidadDeFormas n > 1

--esMuyEspecial :: Integer -> Bool

