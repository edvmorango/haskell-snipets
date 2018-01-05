module ExercisesTests (exercisesTests) where

import Test.Hspec
import Test.QuickCheck
import Exercises
import Data.List

genFractional :: (Arbitrary a , Fractional a) => Gen a -- Must extends Arbitrary when wants force a Instance
genFractional = do
  a <- arbitrary
  return a

genIntegral :: (Arbitrary a, Integral a) => Gen a
genIntegral = do
  a <- arbitrary
  return a

genIntegralDividableTuple :: (Arbitrary a, Integral a) => Gen (a,a)
genIntegralDividableTuple = do
  a <- arbitrary `suchThat` (/= 0)
  b <- arbitrary `suchThat` (/= 0)
  return (a,b)

genOrd :: (Arbitrary a, Ord a) =>  Gen a
genOrd = do
  a <- arbitrary
  return a


genSortedList :: (Ord a) => Gen a -> Gen [a]
genSortedList a = do
   l <- listOf a
   return $ sort l

genList :: (Eq a) => Gen a -> Gen [a]
genList a = do
   l <- listOf a
   return l


genNumber :: (Arbitrary a, Eq a, Num a) => Gen a
genNumber = do
  a <- arbitrary
  return a

genPotentiationTuple :: (Arbitrary a
                       , Ord a
                       , Integral a) => Gen (a,a)
genPotentiationTuple = do
  a <- arbitrary `suchThat` (> 1)
  b <- arbitrary `suchThat` (> 1)
  return (a, b)

genPotentiationTuple3 :: (Arbitrary a, Ord a, Integral a) => Gen (a,a,a)
genPotentiationTuple3 = do
  a <- arbitrary  `suchThat` (> 1) `suchThat` (< 10)
  b <- arbitrary  `suchThat` (> 1) `suchThat` (< 10) `suchThat` (/= a)
  c <- arbitrary  `suchThat` (> 1) `suchThat` (< 10) `suchThat` (/= a) `suchThat` (/= b)
  return (a,b,c)


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
  describe "quot & div  properties" $ do
    it "quot" $ do
      forAll (genIntegralDividableTuple :: Gen (Integer, Integer)) (\(x,y)-> (quot x y)*y + (rem x y) == x)
    it "div" $ do
      forAll (genIntegralDividableTuple :: Gen (Integer, Integer)) (\(x,y)-> (div x y)*y + (mod x y) == x)
  describe "^ associative" $ do
    it "is not associative " $ do
      True-- forAll (genPotentiationTuple3 :: Gen (Integer, Integer, Integer)) (\(a,b,c) -> a ^ (b ^ c) /=  (a ^ b) ^ c  )
  describe "reverse identty" $ do 
    it "(reverse . reverse) x = x" $ do
      forAll (genList (genNumber :: Gen Int)) (\x -> reverse (reverse x) == x )



----
