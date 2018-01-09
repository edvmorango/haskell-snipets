module Spec where

import Test.Hspec
import Test.QuickCheck
import Data.Monoid


genThreple :: (Arbitrary a, Monoid a) => Gen (a,a,a)
genThreple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ (a,b,c)


genMonoid :: (Arbitrary a, Monoid a) => Gen a
genMonoid = do
  a <- arbitrary
  return a



main :: IO ()
main = hspec $ do
  describe "monoids" $ do
    it "associativity " $ do
      forAll (genThreple ::  Gen (Sum Int, Sum Int, Sum Int))
        (\(a,b,c)  ->  (a <> b) <> c == a <> (b <> c))
    it "associativity unitary" $ do
     ((Sum 1 <> Sum 2) <> Sum 3) `shouldBe` (Sum 1 <> (Sum 2 <> Sum 3))
    it "right identity" $ do
      forAll (genMonoid :: Gen (Product Int)) (\a -> a <> mempty == a )
    it "left identity" $ do
      forAll (genMonoid :: Gen (Product Int)) (\a -> mempty <> a == a )
