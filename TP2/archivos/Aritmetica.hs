module Aritmetica where
import Tipos
import Data.Tuple
import Data.Bits


--(1)
mcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
mcdExt a 0 = (abs a, 1, 2)
mcdExt a b = (d, t, s - q * t)
  where (d, s, t) = mcdExt b (a `mod` b)
        q = div a b

--(2)
-- Nuestro critero para determinar los primos de la criba es que su cantidad de divisores sean iguales a 2. De esta forma, por ejemplo, 1 no cuenta como primo. )
criba :: Integer -> Set Integer
criba 1 = []
criba n | esPrimo (n-1) = (n-1):(criba (n-1))
        | otherwise = criba (n-1)

esPrimo :: Integer -> Bool
esPrimo n = length (divisores n) == 2

divisores :: Integer -> Set Integer
divisores n = divisoresAux n n 

divisoresAux :: Integer -> Integer -> Set Integer
divisoresAux n 0 = []
divisoresAux n k | mod n k == 0 = k : divisoresAux n (k-1)
                 | otherwise = divisoresAux n (k-1)

--(3)
--Un numero es coprimo con otro si su mcd da 1. Buscamos un primo menor entonces restamos 1 al valor recibido.
coprimoCon :: Integer -> Integer
coprimoCon 1 = 0
coprimoCon n | mcd n (n-1) == 1 = (n-1)
             | otherwise = coprimoCon (n-1)

mcd :: Integer -> Integer -> Integer
mcd n 0 = n
mcd n k = mcd k (mod n k)

--(4)
-- Si n y m son coprimos. Usa el algoritmo de eucliedes extendido para obtener los valores que arman el mcd (n.s + m.t = mcd) y calcula el resto de S y M teniendo en cuenta que el resto de N.S y M debe ser 1.
inversoMultiplicativo :: Integer -> Integer -> Integer
inversoMultiplicativo n m | mcd n m == 1 = mod s m
                          | otherwise = error "no son coprimos"
  where s = segundoValorTupla (mcdExt n m)

segundoValorTupla :: (Integer, Integer, Integer) -> Integer
segundoValorTupla (_,x,_) = x

-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1
