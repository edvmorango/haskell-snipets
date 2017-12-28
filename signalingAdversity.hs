module SignalingAdversity where

import Data.Maybe
import Data.List
import Data.Char

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a = Just a

-- split :: (Eq a) => a -> [a] -> [a]
-- split _ [] = []
-- split del lis =  takeWhile (/=del) lis : split del next
  -- where next = (drop 1 $ dropWhile(/=del) lis)

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
