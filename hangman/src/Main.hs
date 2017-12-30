module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)


newtype WordList = WordList [String] deriving (Eq, Show)

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered tries) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guesse so far: " ++ tries



freshPuzzle :: String -> Puzzle
freshPuzzle w =  Puzzle w  (map (const Nothing) w) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _ ) c =  elem c w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g ) c =  elem c g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just a) = a
renderPuzzleChar Nothing = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle w h g) c = Puzzle w newFilledInSoFar (c : g)
      where zipper guessed wordChar guessChar =
              if wordChar == guessed
                then Just wordChar
                else guessChar
            newFilledInSoFar = zipWith (zipper c) w h


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True)   -> do putStrLn "Already guessed"
                      return puzzle
    (True, _)   -> do putStrLn "Right guess"
                      return $ fillInCharacter puzzle guess
    (False, _ ) -> do putStrLn "Wrong guess"
                      return $ fillInCharacter puzzle guess


gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 7
    then do putStrLn "You lose!"
            putStrLn $ "The word was: " ++ wordToGuess
            exitSuccess
    else return ()


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
    then do putStrLn "You win!"
            exitSuccess
    else return ()

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/words.txt"
  return $ WordList (lines dict)

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
     [c] -> handleGuess puzzle c >>= runGame
     _ -> putStrLn "Your guess must be a single character"

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter ct aw)
  where ct =  (\a -> let b = length a
                        in
                        if b >= minWordLength && b < maxWordLength
                        then True
                        else False)

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0 , (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

main :: IO ()
main = do
  word <- randomWord'
  let puzzle =
        freshPuzzle (fmap toLower word)
  runGame puzzle
