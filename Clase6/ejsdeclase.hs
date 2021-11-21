main = undefined

ganadores :: [Integer] -> [Integer] -> [Integer]
ganadores a b | a == [] || b == [] = []
              | head a > head b = head a : ganadores (tail a) (tail b)
              | otherwise = head b : ganadores (tail a) (tail b)

elementosEntre :: Integer -> Integer -> [a] -> [a]
elementosEntre a b c | 
 -- quiero delimitar el primero y el ultimo
 -- el primero es head
 --
 -- funcion que da el ultimo elemento de una lista
 ultimoElemento :: [Int] -> Int
ultimoElemento a | a == [a] = ultimoElemento
                 | otherwise = (tail ultimoElemento a)  
