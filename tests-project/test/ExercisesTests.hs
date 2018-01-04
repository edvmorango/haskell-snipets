module ExercisesTests (exercisesTests) where

import Test.Hspec
import Test.QuickCheck
import Exercises


genFractional :: (Arbitrary a , Fractional a) => Gen a -- Must extends Arbitrary when wants force a Instance
genFractional = do
  a <- arbitrary
  return a

genOrd :: (Arbitrary a, Ord a) =>  Gen a
genOrd = do
  a <- arbitrary
  return a

genListOfOrd :: (Ord a) => Gen a -> Gen [a]
genListOfOrd a = listOf a


exercisesTests :: IO ()
exercisesTests = hspec $ do
  describe "half" $ do
    it "half N = N / 2" $ do
      forAll (genFractional :: Gen Double ) (\x ->  half x == (x / 2) )
  describe "halfIdentity" $ do
    it "halfIdentity N = N " $ do
      forAll (genFractional :: Gen Double ) (\x -> halfIdentity x ==  x )
  describe "listOrdered" $ do
    it "listOrdered == True when ordered" $ do
      forAll (genListOfOrd (genOrd :: Gen Int) ) (\x -> listOrdered x  == True)
