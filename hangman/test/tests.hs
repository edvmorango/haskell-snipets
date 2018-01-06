module MainTest where

import Hangman
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = tests


genT ::  (Arbitrary a, Eq a, Ord a) => Gen a
genT = do
  a <- arbitrary
  return a

genNaturalUntil :: Int -> Gen Int
genNaturalUntil 0 = return 0
genNaturalUntil a = elements [0..(a - 1)]

genPuzzleChar :: Gen (Puzzle, Char)
genPuzzleChar = do
  word <- (genT :: (Gen String))  `suchThat` (/= "")
  index <-  genNaturalUntil (length word)
  let hits = map (\_ -> Nothing) word
      puzzle =  Puzzle word hits []
      char = word !! index in
    return  ( puzzle, char )

isFilled :: (Puzzle, Char) -> Bool
isFilled (p,c) = result newPuzzle
  where newPuzzle = fillInCharacter p c
        result (Puzzle _ hits _)  = elem (Just c) hits


tests :: IO ()
tests =  hspec $ do
  describe "hangman" $ do
    it "should fill character if exists" $ do
      forAll genPuzzleChar isFilled
    it "should handle the guess " $ do
      True
