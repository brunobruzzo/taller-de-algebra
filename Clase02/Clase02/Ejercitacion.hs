f1 x y z = x**y + z <= x+y**z
 
f2 x y = (sqrt x) / (sqrt y)

f3 x y = div (sqrt x) (sqrt y)
 
f4 x y z | x == y = z
         | x ** y == y = z
         | otherwise = z
          
f5 x y z | x == y = z
         | x ** y == y = x
         | otherwise = y
      
cinco :: Int
cinco = 5
--f3 cinco cinco
