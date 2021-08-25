-- Dados dos numeros reales, decidi si estan relacionados considerando la relacion de equivalencia (-inf,3],(3,7],(7,inf)

estanRelacionados :: Float -> Float -> Bool
estanRelacionados x y = (x<=3 && y<=3) || (x>3 && y>3 && x <=7 && y <=7) || (x>7 && y>7)

-- Calcula el producto interno para dos vectores de R2

prodInt :: (Float,Float) -> (Float,Float) -> Float
prodInt (x1,y1) (x2,y2) = x1*x2 + y1*y2

-- Dados dos vectores, decide si es cierto que cada coordenada del primer vector es menor a la coordenada correspondiente del segundo vector.

todoMenor :: (Float,Float) -> (Float,Float) -> Bool
todoMenor (x1,y1) (x2,y2) = x1<x2 && y1<y2

-- Calcula la distancia entre dos puntos de R2
distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos (x1,y1) (x2,y2) = sqrt(((x1-x2)^2)+((y1-y2)^2))

-- Dada una terna de enteros, calcular la suma de sus tres elementos.
sumaTerna :: (Int,Int,Int) -> Int
sumaTerna (x,y,z) = x+y+z

-- Dada una terna de enteros, devuelve la posicion del primer numero par si es que hay alguno, y devuelve 4 si son todos impares.
posicPrimerPar :: (Int,Int,Int) -> Int
posicPrimerPar (x,y,z) | mod x 2 == 0
                       | otherwise = 4
--			where terna = (x,y,z) 

-- Crea un par a partir de las dos componentes dadas por separado (pueden ser de cualquier tipo)
crearPar :: a -> b -> (a, b)
crearPar x y = (x, y)

-- Invierte los elementos del par pasado como parametro (pueden ser de cualquier tipo)
invertir :: (a, b) -> (b, a)
invertir (x, y) = (y, x)
