module SignalingAdversity where

import Data.Maybe
import Data.List
import Data.Char

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a = Just a

split :: Char -> String -> [String]
split _ [] = []
split del s = takeWhile (/=del) s : split del next
  where next = (drop 1 $ dropWhile(/=del) s)


splitBySpace :: String -> [String]
splitBySpace = split ' '

replaceThe :: String -> String
replaceThe [] = []
replaceThe v =  ( unwords . map (fromMaybe "a" . notThe) . splitBySpace) v

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel w = (foldr lambda 0 . splitBySpace) w
  where lambda = (\a acc -> case (notThe a) of
                            Just _ -> acc
                            Nothing -> acc + 1)

isVowel :: Char -> Bool
isVowel = ( (flip elem "aeiou") .  toLower)

countVowels :: String -> Integer
countVowels = foldr (\a acc ->  if isVowel a then acc + 1 else acc) 0

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String ->  Maybe Word'
mkWord s = if a > b
         then Nothing
         else Just (Word' s)
  where (a,b) = foldr (\a (v,c) -> if (isVowel a) then (v+1,c) else (v, c+1)) (0,0) s

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger (Zero) = 0
natToInteger (Succ a) = 1 + natToInteger a

integerToNat :: Integer -> Maybe Nat
integerToNat a
  | a < 0 = Nothing
  | otherwise = Just $ parse a
              where parse 0 = Zero
                    parse p = Succ $ parse (p-1)

--
