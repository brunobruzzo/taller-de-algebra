main = undefined
type Set a = [a]

vacio :: Set [a]
vacio = []

pertenece :: Int -> Set Int -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys

perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC xs [] = False
perteneceC xs (ys:yss) = iguales xs ys || perteneceC xs yss

agregar :: Int -> Set Int -> Set Int
agregar x c | pertenece x c = c
            | otherwise = x:c

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss | perteneceC xs xss = xss
                | otherwise = xs:xss

incluido :: Set Int -> Set Int -> Bool
incluido [] c = True
incluido (x:xs) c = pertenece x c && incluido xs c

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

union :: Set Int -> Set Int -> Set Int
union [] c = c
union (x:xs) c | pertenece x c = union xs c
               | otherwise = x:union xs c 

interseccion :: Set Int -> Set Int -> Set Int
interseccion [] c = []
interseccion (x:xs) c | pertenece x c = x:interseccion xs c
                      | otherwise = interseccion xs c 

diferencia :: Set Int -> Set Int -> Set Int
diferencia [] c = []
diferencia (x:xs) c | pertenece x c = diferencia xs c
                    | otherwise = x:diferencia xs c

diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica a b = union (diferencia a b) (diferencia b a)

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos x [] = []
agregarATodos x (c:cs) = agregarC (agregar x c) (agregarATodos x cs)

unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC [] c = c
unionC (x:xs) c | perteneceC x c = unionC xs c
                | otherwise = x:unionC xs c

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (x:xs) = unionC (partes xs) (agregarATodos x (partes xs))

conjuntoN :: Int -> Set Int
conjuntoN 0 = []
conjuntoN n = n : conjuntoN (n-1)

partesN :: Int -> Set (Set Int)
partesN n = partes (conjuntoN n)

productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano (x:xs) [] = []
productoCartesiano (x:xs) (y:ys) = (x,y):productoCartesiano (x:xs) ys 

productoCartesianoFila :: Int -> Set Int -> Set (Int,Int)

iteradorFilasProdCart :: Set Int -> Set Int -> Set(Int,Int)
iteradorFilasProdCart (x:xs) ys = xs
--A=[1,2,3] B=[3,4]
-- (1,3):(1,4) 123 []  
{-
unionConjTupla :: Set (Int, Int) -> Set (Int, Int) -> Set(Int,Int)
unionConjTupla [] c = c
unionConjTupla (x:xs) c | perteneceConjTupla x c = unionConjTupla xs c
                        | otherwise = x:unionConjTupla xs c

perteneceConjTupla :: (Int,Int) -> Set (Int, Int) -> Bool
perteneceConjTupla xs [] = False
perteneceConjTupla xs (ys:yss) = iguales xs ys || perteneceConjTupla xs ys

igualesConjTupla-}
