module RSA where
import Tipos
import Aritmetica


--(3)
-- e es cualquier numero menor a (p-1)(q-1), nosotros elegimos tomar el siguiente menor. 
claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = (e, d, n)
  where n = p*q
        e = coprimoCon m
        m = (p-1)*(q-1)
        d = inversoMultiplicativo e m
 
 
{- descomposicion de funcion clave d 
 
       como e.d ~congruente~ 1 (mod m) <=> IM.e.d ~congruente~ IM (mod m) <=> d ~congruente~ IM (mod m) 
  entonces => mod d - IM e m == 0

donde IM es el inverso multiplicativo. 
-}
                 
--(6)
-- traduce el mensaje a numeros. Luego, toma el primer valor y lo codifica usando modExp (por definicion). Agrega el valor a la lista vacia y sigue la recurrencia hasta vaciar mensaje.
codificador :: Clpub -> Mensaje -> Cifrado
codificador (e,n) m | mensaje == [] = [] 
                    | mcd (head mensaje) n /= 1 = (-(head mensaje):(codificador (e,n) (tail m)))
                    | otherwise = (modExp (head mensaje) e n):(codificador (e,n) (tail m))
 where mensaje = aEnteros m 

--(7)
-- traduce el cifrado decodificicado en la funcion auxiliar a ASCII.
decodificador :: Clpri -> Cifrado -> Mensaje
decodificador (d,n) c = aChars (decodificadorAux (d,n) c)

-- Toma el primer valor de la lista y lo decodifica usando modExp (por definicion). Agrega el valor a la lista vacia y sigue la recurrencia hasta vaciar el mensaje. La lista debe ser traducida por la funcion decoficidador.
decodificadorAux :: Clpri -> Cifrado -> Set Integer
decodificadorAux (d,n) c | c == [] = []
                         | head c < 0 = (-(head c):(decodificadorAux (d,n) (tail c)))
                         | otherwise = (modExp (head c) d n):(decodificadorAux (d,n) (tail c)) 
