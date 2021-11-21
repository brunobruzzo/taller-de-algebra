main = undefined

digitos :: Integer -> Integer -> [Integer]
digitos 0 b = []
digitos n b = (mod n b) : (digitos (div n b) b) 

numeroAux :: [Integer] -> Integer -> Integer -> Integer
numeroAux [] b k = 0
numeroAux lista b k = (head lista)*b^k + numeroAux (tail lista) b (k+1)

numero :: [Integer] -> Integer -> Integer
numero lista b = numeroAux lista b 0

divisores :: Int -> [Int]
divisores n = divisoresAux n n

divisoresAux :: Int -> Int -> [Int]
divisoresAux n 0 = []
divisoresAux n k | mod n k == 0 = k : divisoresAux n (k-1)
                 | otherwise = divisoresAux n (k-1)

interseccion :: Eq a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion (x:xs) ys
    | x `elem` ys = x : interseccion xs ys
    | otherwise = interseccion xs ys

mcdDef :: Int -> Int -> Int
mcdDef a 0 = abs a
mcdDef 0 b = abs b
mcdDef a b = maximum (interseccion (divisores a) (divisores b))

mcd :: Int -> Int -> Int
mcd n 0 = n
mcd n k = mcd k (mod n k)

--mcm :: Int -> Int -> Int

esPrimo :: Int -> Bool
esPrimo n = length (divisores n) == 2 

criba :: Int -> [Int]
criba 1 = []
criba n | esPrimo (n-1) = (n-1):(criba (n-1))
        | otherwise = criba (n-1)
