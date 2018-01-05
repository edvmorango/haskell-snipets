module ExercisesTests (exercisesTests) where

import Test.Hspec
import Test.QuickCheck
import Exercises
import Data.List

genFractional :: (Arbitrary a , Fractional a) => Gen a -- Must extends Arbitrary when wants force a Instance
genFractional = do
  a <- arbitrary
  return a

genOrd :: (Arbitrary a, Ord a) =>  Gen a
genOrd = do
  a <- arbitrary
  return a

genSortedList :: (Ord a) => Gen a -> Gen [a]
genSortedList a = do
   l <- listOf a
   return $ sort l

genNumber :: (Arbitrary a, Eq a, Num a) => Gen a
genNumber = do
  a <- arbitrary
  return a



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
      forAll (genSortedList (genOrd :: Gen Int)) (\x -> listOrdered x)
  describe "plusAssociative" $ do
    it "plusAssociative a b c = true" $ do
      forAll (genNumber :: Gen Int) (\x y z -> plusAssociative x y z)
  describe "plusCommutative" $ do
    it "plusCommutative a b c = true" $ do
      forAll (genNumber :: Gen Int) (\x y -> plusCommutative x y)
  describe "productAssociative" $ do
    it "productAssociative a b c = true" $ do
      forAll (genNumber :: Gen Int) (\x y z -> productAssociative x y z)
  describe "productCommutative" $ do
    it "productCommutative a b c = true" $ do
      forAll (genNumber :: Gen Int) (\x y -> productCommutative x y)
