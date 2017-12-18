module Patterns where


f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f  (a, _, c) (d, _, f)  = ((a, d), (c, f))


functionC x y = case (x > y) of
                True -> x
                False -> y

ifEvenAdd2 n = case (even n) of
               True -> (n+2)
               False ->  n

nums x = case compare x 0 of
        LT -> -1
        GT -> 1
        _ -> 0

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2



avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
 |y>=0.9 ='A'
 |y>=0.8 ='B'
 |y>=0.7 ='C'
 |y >= 0.59 = 'D'
 | otherwise='F'
 where y = x / 100

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)




tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool a b c = case c of
                 False -> a
                 True -> b

foldBool2 :: a -> a -> Bool -> a
foldBool2 a b c
 | c == False = a
 | otherwise = b

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) =  (f a, c)



roundTrip :: (Show a, Read b) => a -> b
roundTrip a = (read . show) a

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = (read . show)


main = do
  print (roundTripPF 4)
  print (id 4)
