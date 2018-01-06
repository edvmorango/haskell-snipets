module MainTest where

import Hangman
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
main = tests


genT ::  (Arbitrary a, Eq a, Ord a) => Gen a
genT = do
  a <- arbitrary
  return a

genNaturalUntil :: Int -> Gen Int
genNaturalUntil 0 = return 0
genNaturalUntil a = elements [0..(a - 1)]

genValidCharacter :: Gen Char
genValidCharacter = elements ['a'..'z']

genWord :: Gen String
genWord = listOf (genValidCharacter) `suchThat` (\x -> let s = (length x) in  s >= minWordLength && s <= maxWordLength)

defaultHits :: String -> [Maybe Char]
defaultHits word = map (\_ -> Nothing) word

genPuzzleChar :: Gen (Puzzle, Char)
genPuzzleChar = do
  word <- genWord  `suchThat` (/= "")
  index <- genNaturalUntil (length word)
  let puzzle = Puzzle word (defaultHits word) []
      char = word !! index in
    return  ( puzzle, char )

genPuzzleWrongChar :: Gen (Puzzle, Char)
genPuzzleWrongChar = do
  word <- genWord  `suchThat` (/= "")
  char <- genValidCharacter `suchThat` (not . flip elem word)
  let puzzle = Puzzle word (defaultHits word) []
    in return  (puzzle, char )


fillCharacters :: String -> Puzzle -> Puzzle
fillCharacters h p = foldr (\c w -> fillInCharacter w c) p h


genRightPuzzle :: Gen Puzzle
genRightPuzzle = do
  word <- genWord  `suchThat` (/= "")
  index <-  genNaturalUntil (length word)
  wrong <-   (listOf ((genValidCharacter) `suchThat` (\x -> not (elem x word))))  `suchThat` (\x -> length x < length word)
  return $ fillCharacters word (Puzzle word (map (Just) word) wrong)

genWrongPuzzle :: Gen Puzzle
genWrongPuzzle = do
  word <- genWord  `suchThat` (/= "")
  index <- genNaturalUntil (length word)
  hits <- listOf (genValidCharacter `suchThat` (\x -> elem x word)) `suchThat` (\x -> length x < length word)
  wrong <- (listOf ((genValidCharacter) `suchThat` (\x -> not (elem x word))))  `suchThat` (\x -> length x == length word)
  return $ fillCharacters hits (Puzzle word (defaultHits word) wrong)

genEmptyPuzzle :: Puzzle -> Puzzle
genEmptyPuzzle (Puzzle w _ _) = Puzzle w (defaultHits w) []

isFilled :: (Puzzle, Char) -> Bool
isFilled (p,c) = result newPuzzle
  where newPuzzle = fillInCharacter p c
        result (Puzzle _ hits _)  = elem (Just c) hits

handleRightGuess :: (Puzzle, Char) -> Property
handleRightGuess (p,c) = monadicIO $ do
  a <- run (handleGuess p c)
  assert(response a)
    where response (Puzzle _ hits _) = elem (Just c) hits

handleWrongGuess :: (Puzzle, Char) -> Property
handleWrongGuess (p,c) = monadicIO $ do
  a <- run (handleGuess p c)
  assert(response a)
  where response (Puzzle _ hits guess) =  not (elem (Just c) hits)  && (elem c guess)

tests :: IO ()
tests =  hspec $ do
  describe "hangman" $ do
    it "should fill character if exists" $ do
      forAll genPuzzleChar isFilled
    it "should handle right guess " $ do
      forAll genPuzzleChar handleRightGuess
    it "should handle wrong guess " $ do
      forAll genPuzzleWrongChar handleWrongGuess
