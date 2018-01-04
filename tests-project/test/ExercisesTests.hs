module ExercisesTests (exercisesTests) where

import Test.Hspec
import Test.QuickCheck
import Exercises




exercisesTests :: IO ()
exercisesTests = hspec $ do
  describe "half" $ do
    it "half N = N / 2" $ do
      forAll arbitrarySizedFractional (\x ->  half x == (x / 2) )
