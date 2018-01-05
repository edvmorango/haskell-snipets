module Exercises where

import Data.List
import Data.Char

data Fool =  Fulse | Frue deriving (Eq, Show)

half :: (Fractional a) => a -> a
half x = x / 2

halfIdentity :: (Fractional a) => a -> a
halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)


plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

productAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
productAssociative x y z = x * (y * z) == (x * y) * z

productCommutative :: (Eq a, Num a) => a -> a -> Bool
productCommutative x y = x * y == y * x

capitalize :: String -> String
capitalize [] = []
capitalize (a:t) =  toUpper a : t

twice f = f . f

fourTimes = (twice . twice)
