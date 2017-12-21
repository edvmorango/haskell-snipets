module Folders where

import Data.Time
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
     (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 1
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate db =  foldr filtr [] db
  where filtr a b =  case a of
                    DbDate time -> time : b
                    _ -> b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber db = foldr filtr [] db
  where filtr a b =  case a of
                    DbNumber n -> n : b
                    _ ->  b

getMostRecent :: [DatabaseItem] -> UTCTime
getMostRecent = (maximum . filterDbDate)

sumDb :: [DatabaseItem] -> Integer
sumDb = (sum . filterDbNumber)


avgDb :: [DatabaseItem] -> Double
avgDb db =  (fromIntegral $ sum numbers) / (fromIntegral $ length numbers)
  where numbers = filterDbNumber db

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibs20 = take 20 fibs

fibsLessThan100 = takeWhile (<100) fibs

-------------------

stops  = "pbtdkg"
vowels = "aeiou"

stopVowels = [ (a,b,c) |  a <- stops, b <- vowels ,c <- stops  ]
stopVowelsP = [ (a,b,c) |  a <- stops, b <- vowels , c <- stops, a == 'p' ]

substantive = ["Cat", "Dog", "Fish"]
verb = ["Eats", "Plays", "Swims"]

sentences = [ (a,b,c) |  a <- substantive, b <- verb ,c <- substantive  ]


seekritFunc x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f v = foldr (\a b -> (f a) || b ) False v



myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e v =  foldr (\a b -> a == e || b ) False v

myElem' e v = myAny (\a -> a == e) v

myReverse :: [a] -> [a]
myReverse v = foldl (\a b -> b : a ) [] v

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f v = foldr (\a b -> f a : b) [] v

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f v =
  foldr (\a b ->  if f a == True then  a : b else b) [] v

squish :: [[a]] -> [a]
squish v = foldl (\a b -> a ++ b ) [] v

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f v = foldr (\ a b -> (f a) ++ b ) [] v

squishAgain :: [[a]] -> [a]
squishAgain = squish

--myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
--myMaximumBy f v =  foldr (\a b -> if (f a b) == GT then a else b ) (head v) v
