main = undefined

-- Devuelve la productoria de los elementos
productoria :: [Int] -> Int
productoria lista | lista == [] = 1
                  | otherwise = (head lista) * (productoria (tail lista))

-- dado un numero N y una lista xs, suma N a cada elemento de xs.
sumarN :: Int -> [Int] -> [Int]
sumarN n lista | lista == [] = []
            | otherwise =  (n + (head lista)) : sumarN n (tail lista)

-- dada una lista no vacia xs, suma el primer elto a cada elto de xs.
sumarElPrimero :: [Int] -> [Int]
sumarElPrimero lista = sumarN (primerValorLista lista) lista

-- Saca el primer elemento de una lista y lo entrega como entero
primerValorLista  :: [Int] -> Int
primerValorLista (x:xs) = x

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = sumatoria xs + x

longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo lista = sumarN (ultimoValorLista lista) lista

ultimoValorLista :: [Int] -> Int
ultimoValorLista lista | longitud lista == 1 = head(lista)
                       | longitud lista > 1 = ultimoValorLista (tail lista)

-- Entrega solo los pares de la lista
pares :: [Int] -> [Int]
pares lista | lista == [] = []
            | mod (head lista) 2 == 0 = (head lista) : pares (tail lista)
            | otherwise = pares (tail lista)

--quita todos los elementos de la lista iguales a n
quitarTodas :: Int -> [Int] -> [Int]
quitarTodas n lista | lista == [] = []
               | n == (head lista) = quitarTodas n (tail lista)
               | otherwise = (head lista) : (quitarTodas n (tail lista))

-- chequea si un valor n (vamos a usar la funcion primervalorlista) esta en la lista.
pertAux :: Int -> [Int] -> Bool
pertAux n (x:xs) | xs == [] = False
                 | n == (head xs)  = True 
                 | otherwise = pertAux n xs

-- usa partAux con primervalorlista y lo quequa en la lista. sino recurre con la tail.
hayRepetidos :: [Int] -> Bool
hayRepetidos (x:xs) | xs == [] = False
                    | pertAux (primerValorLista (x:xs)) (x:xs) = True
                    | otherwise = hayRepetidos xs  

pertenece :: Int -> [Int] -> Bool
pertenece x l | l == [] = False
              | otherwise = (x == head l ) || pertenece x (tail l)

quitar :: Int -> [Int] -> [Int]
quitar n l | l == [] = []
           | n == (head l) = tail l 
           | otherwise = (head l):(quitar n (tail l))

agregar :: Int -> [Int] -> [Int]
agregar x c | pertenece x c = c
            | otherwise = x:c

eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal [] = []
eliminarRepetidosAlFinal (x:xs) | pertenece x xs = x:eliminarRepetidosAlFinal(quitarTodas x xs)
                                | otherwise = x:eliminarRepetidosAlFinal xs

eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio [] = []
eliminarRepetidosAlInicio (x:xs) | pertenece x xs = eliminarRepetidosAlInicio xs
                                 | otherwise = x:eliminarRepetidosAlFinal xs

maximo :: [Int] -> Int
maximo (x:xs) | xs == [] = x
maximo (x:xs) | x < (maximo xs) = maximo xs
              | otherwise = x

ordenar :: [Int] -> [Int]
ordenar l | tail l == [] = l
          | otherwise = minimo l : ordenar (quitar (minimo l) l)

minimo :: [Int] -> Int
minimo (x:xs) | xs == [] = x
              | otherwise = min x (minimo xs)
