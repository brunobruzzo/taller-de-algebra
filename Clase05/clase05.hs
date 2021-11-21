main = undefined
--sumaDivisores :: Int -> Int


{- suma los divisores de un entero positivo. 
 
ej: sumaDivisores 6 = 1 + 2 + 3 + 6 = 12

Vamos a usar una funcion auxiliar.


sumaDivisoresHasta :: Int -> Int -> Int

 suma los divisores hasta cierto punto de un numero


los d con OK satisfacen la condicion

d = 1 OK
  = 2
  = 3 OK
  = 4
  = 
  .
  .
  .
  = k -1
_______ hasta k -1
  = k

A esto le agregamos 0 si K no divide a  n. le agregamos k si k divide a n

sumaDivisoresHasta m (k-1) (SUMA LOS QUE TIENEN OK)

El caso BASE es cuando k = 1


 -}
sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n k | k == 1 = 1
                       | mod n k == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise = sumaDivisoresHasta n (k-1)

{- ej sumaDivisoresHasta 6 3 | 6 == 1 = 1 (NO, entonces sigue)
                             | mod 6 3 == 0 =  3 + sumaDivisoresHasta 6 2 
                       Como  el resto de 6 / 3 es 0, entonces hace 3 + sumaDivisoresHasta 6 2
                                                   3 + mod 6 2 == 0 = 2 + sumaDivosresHasta 6 1
                                                                          como k == 1 = 1
                        				3 + 2 + 1
-}
--Ahora podemos usar la funcion con una sola variable n, y la damos dos veces en la funcion aux.
sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n 



{-- Lo podemos pensar, en vez de hasta, desde.
 Sumadivisoresdesde n k es igiual a la suma desde los divisores de n (d/n) que sean mayores que K. 


d = k 
----------- desde k + 1
  = k + 1
  .
  .
  .
  .
  = d

esto es sumaDivisoresDesde n k+1 
hay que ver cuales son divisores o no
0 si el k no divide a n
k si el k divide a n



-}
sumaDivisoresDesde :: Int -> Int -> Int
sumaDivisoresDesde n k | k == n = n
                       | mod n k == 0 = k + sumaDivisoresDesde n (k+1)
                       | otherwise = sumaDivisoresDesde n (k+1)
-- Ahora habria que cambiar sumaDivisores para esta func aux


