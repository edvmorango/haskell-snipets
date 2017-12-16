module TypeInference where

f :: Num a => a -> a -> a
f x y = x + y + 3


f2 x y = x + y + 3


myConcat x = x ++ " Oi"

myMult x = (x / 3) * 5

myCom x = x > (length [1..10])

myAlph x = x < 'z'


test x y z = x + y + z
