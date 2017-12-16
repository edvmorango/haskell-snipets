{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineType where
import Data.Char (intToDigit)


example =  1

v1 = ( * 9 ) 6

v2 = head [(0,"doge"),(1,"kitteh")]
v3 = head [(0 :: Integer ,"doge"),(1,"kitteh")]
v4 = if False then True else False
v5 = length [1, 2, 3, 4, 5]
v6 = (length [1, 2, 3, 4]) > (length "TACOCAT")

-- x = 5
-- y = x + 5
-- z y = y * 10

-- x = "Julie"
-- y = " <3 "
-- z = "Haskell"
-- f = x ++ y ++ z


bigNum = (^) 5 $ 2
-- wahoo = bigNum $ 10

-- x = print
-- y = print "woohoo!"
-- z = x "hello world"

-- a = (+)
-- b = 5
-- c = a b 10
-- d = a c 200


functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) =>  a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y



--1. There is only one function definition that typechecks and doesn’t
--go into an infinite loop when you run it.
i :: a -> a
i a = a
--2. There is only one version that works.
c :: a -> b -> a
c a _ = a
--3. Given alpha equivalence are c'' and c (see above) the same
--thing?
c'' :: b -> a -> b
c'' b _ = b
--4. Only one version that works.
c' :: a -> b -> b
c' _ b = b
--5. There are multiple possibilities, at least two of which you’ve
--seen in previous chapters.
r :: [a] -> [a]
r (_:xs) = xs
--6. Only one version that will typecheck.
co :: (b -> c) -> (a -> b) -> a -> c
co bC aB a = bC $ aB a

a7 :: (a -> c) -> a -> a
a7 _ x = x

a' :: (a -> b) -> a -> b
a' aB a = aB a



fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y) then fstString x else sndString y
       where x = "Singin"
             y = "Somewhere"

main :: IO ()
main = do
  print (1 + 2)
  putStrLn ("10")
  print $ negate 1
  print ((+) 0 blah)
  where blah = negate 1


f :: Int -> String
f a = show a

g :: String -> Char
g (x:_) = x

h :: Int -> Char
h x = intToDigit x


data A
data B
data C

q :: A -> B
q = undefined
w :: B -> C
w = undefined

e :: A -> C
e a = w $ q a

data X
data Y
data Z

xz :: X -> Z
xz = undefined
yz :: Y -> Z
yz = undefined
xform :: (X, Y) -> (Z, Z)
xform (x,y) = (xz x,yz y)

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xY yWZ x = fst $ yWZ (xY x)
