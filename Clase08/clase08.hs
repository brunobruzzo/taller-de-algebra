main = undefined

type Set a = [a]

vacio :: Set a
vacio = []

agregar :: Eq a => a -> Set a -> Set a
agregar n c | n `elem` c = c
            | otherwise = n:c

union :: Eq a => Set a -> Set a -> Set a
union [] ys = ys
union (x:xs) ys = union xs (agregar x ys)

---------------------------


agregarElementoAdelante :: Int -> Set [Int] -> Set [Int]
agregarElementoAdelante x [] = []
agregarElementoAdelante x (ys:yss) = agregar (x:ys) (agregarElementoAdelante x yss)

agregarElementoAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElementoAListas [] _ = []
agregarElementoAListas (x:xs) c = (agregarElementoAdelante x c) `union` (agregarElementoAListas xs c)

insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn xs n i | i == 1 = n:xs 
                  | otherwise = (head xs) : (insertarEn (tail xs) n (i-1))

variaciones :: Set Int -> Int -> Set [Int]
variaciones c 0 = [[]]
variaciones c k = agregarElementoAListas c (variaciones c (k-1))

--bolitasEnCajas :: Int -> Int -> Set [Int]
-- la cantidad de bolitas que puedo meter en k cajas es el factorial de (n k).
-- quiero armar el conjunto. es decir si tengo 2 bolitas en 3 cajas
-- bolita 1 puede ir a caja 1, bolita 2 puede ocupar el otro lugar de la caja 1.
-- bolita 1 puede ir a caja 1, bolita 2 puede ir a caja 2
-- bolita 1 puede ir a caja 2, bolita 2 puede ir a caja 1.
--       ...      ...     ....   ....    ....
--       ...      ...     ....   ....    ....
-- bolita n1 puede ir a caja k1, bolita k2 puede ir a caja k2
--
--
-- la cantidad de elementos dentro de cada conjunto depende de la cantidad de bolitas. es un factorial
--
-- bolitasEnCajas 2 3
-- bolitasEnCajas hay dos bolitas y 3 cajas. bolita 1 puede ir a caja 1 2 o 3.
-- 3:agregarElemento
--
armarLista :: Int -> Set [Int]
armarLista 0 = []
armarLista n = [n]:armarLista(n-1)

agregarALista :: Int -> Set [Int] -> Set [Int]
agregarALista n [] = []
agregarALista n (xs:xss) = (n:xs):(agregarALista n xss)

armarConjuntoBolitas :: Int -> Set [Int] -> Set [Int]
armarConjuntoBolitas 0 (xs:xss) = []
armarConjuntoBolitas n (xs:xss) = agregarALista n (armarLista n) `union` armarConjuntoBolitas (n-1) (xs:xss)

bolitasEnCajas :: Int -> Int -> Set [Int]
bolitasEnCajas 0 k = []
bolitasEnCajas n k = agregarALista k (armarLista n) `union` bolitasEnCajas (n-1) k

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

-- el numero combinatorio (n k) es n!/k!(n-k)!
combinatorio :: Int -> Int -> Int
combinatorio n k = div (fact n) ((fact k)*(fact (n-k)))

combinatorio' :: Int -> Int -> Int
combinatorio' n 0 = 1
combinatorio' n k | n == k = 1
                  | otherwise = (combinatorio' (n-1) k) + (combinatorio' (n-1) (k-1))
