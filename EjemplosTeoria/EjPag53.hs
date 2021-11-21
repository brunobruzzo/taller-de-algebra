sumatoria :: (Integer -> Integer) -> Integer -> Integer
sumatoria a 0 = 0
sumatoria a n = a n + sumatoria a (n - 1)
