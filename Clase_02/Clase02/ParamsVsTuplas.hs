 suma :: (Float, Float) -> (Float, Float) -> (Float, Float)
 suma (vx, vy) (wx, wy) = (vx+wx, vy + wy)

 -- |normaVectorial2 x y es la norma de (x,y)
 normaVectorial2 ::  Float -> Float -> Float
 normaVectorial2 x y = sqrt (x^2 + y^2)
 
 -- |normaVectorial1 (x,y) es la norma de (x,y)
 normaVectorial1 ::  (Float, Float) -> Float
 normaVectorial1 (x,y) = sqrt (x^2 + y^2)
 
 norma1Suma :: (Float, Float) -> (Float, Float) -> Float
 norma1Suma v1 v2 = normaVectorial1 (suma v1 v2)
 
 norma2Suma :: (Float, Float) -> (Float, Float) -> Float
 norma2Suma v1 v2 = normaVectorial2 (fst s) (snd s)
     where s = suma v1 v2 
