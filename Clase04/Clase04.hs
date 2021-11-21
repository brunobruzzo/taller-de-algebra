g1 :: Float -> Int -> Float
g1 i n | i == fromIntegral(n) = i^n
g1 i n = (g1 i (n-1)) +i^n

gb2 :: Int -> Int -> Int
gb2 n i | n == i = i^n
gb2 n i =  fromIntegral(gb2(n-1) i) + n^n

gb3 :: Int -> Int -> Int
gb3 1 m = 1
gb3 n m = n^m + gb3(n-1) m

-- Hace la suma de i desde 0 a n de m^n
f :: Int -> Int -> Int
f m 0 = 1
f m n = m^n +f m (n-1)

-- La sumatoria de i desde 1 a n de la sumatoria de j de 1 a n de i^j
--
-- Si miramos la sumatoria de forma matricial se ve de esta manera
--
--
--       1^1 + 1^2 + 1^3 ..1^4      ...      1^n-1  +   1^n
--             2^2 + 2^3 + 2^4      ...      2^n-1  +   2^n
--                   3^3 + 3^4      ...      3^n-1  +   3^n
--                                   
--                            
--                                        + (n-1)^n-1 + (n-1)^n
--                                                          n^n
--
-- Esta escalonada, siempre un cero mas al comienzo de cada fila
--
-- sumaSanguche recibe dos enteros. el 1er numero y hasta que exponente se va a elevar.
-- Calcula sanguAux de ambos numeros.
sumaSanguche :: Int -> Int -> Int
sumaSanguche a n = sanguAux a n

--Calcula todos los valores de una fila que comienza con m
sanguFila :: Int -> Int -> Int
sanguFila m n | m == n = m^m
              | otherwise =  m^n + (sanguFila m (n-1))

-- Hace una recurrencia de cada fila desde a(el primer valor)  hasta n
sanguAux :: Int -> Int -> Int
sanguAux a n | a == n = sanguFila a n
sanguAux a n = (sanguAux(a+1) n) + sanguFila a n

{-
sanguAux 1 4 = sanguAux(2  4) + sanguFila 1 4 
                                1^4 + sanguFila 1 3
                                       1^3 + sanguFila 1 2
                                        1^2 + sanguFila 1 1
                                         1^1             


               sanguAux(3  4) + sanguFila 2 4
                                 2^4 + sanguFila 2 3
                                  2^3 + sanguFila 2 2
                                    2^2


               sanguAux(4  4) +      sanguFila 3 4
                 sanguFila 4 4        3^4 + sanguFila 3 3
                      4^4               3^3
-}
