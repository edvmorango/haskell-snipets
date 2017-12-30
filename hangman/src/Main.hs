module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]


minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/words.txt"
  return $ lines dict

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return $ filter ct aw
  where ct =  (\a -> let b = length a
                        in
                        if b >= minWordLength && b < maxWordLength
                        then True
                        else False)


randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0 , (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord


main :: IO ()
main = do putStrLn "hello world"
