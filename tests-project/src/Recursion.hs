module Recursion where
import Data.List (intersperse, intercalate)
import Data.Char (digitToInt)


nTimes :: (Eq a, Num a) => a -> a
nTimes 0 = 0
nTimes n = n + (nTimes (n - 1))

recMultiplication :: (Integral a) => a -> a -> a
recMultiplication _ 0 = 1
recMultiplication a b =  a  * (recMultiplication a (b-1) )


data DividedResult = Result Integer | Undetermined deriving (Show)

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num denom = go num denom 0
  where go n d count
         | d == 0 = Undetermined
         | n < d = Result count
         | otherwise = go (n - d) d (count + 1)

mc91 :: (Num a, Ord a) => a -> a
mc91 n
 | n > 100 = n - 10
 | otherwise = mc91 $ mc91 (n+11)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "invalid"

digits :: Int -> [Int]
digits n =  ( map digitToInt . show) n


wordNumber :: Int -> String
wordNumber n =   (intercalate "-" . map digitToWord . digits) n
