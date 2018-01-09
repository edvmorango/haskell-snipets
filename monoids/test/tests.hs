module MonoidsTests where

import Test.Hspec
import Test.QuickCheck
import Data.Monoid
import Monoids
import Semigroups

genT :: (Arbitrary a, Eq a) => Gen a
genT = do
  a <- arbitrary
  return a

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

genOptional :: (Arbitrary a) => Gen (Optional a)
genOptional = do
  x <- arbitrary
  elements[ Optional Nada, Optional (Algo x)]

genThrepleOptional :: (Arbitrary a) => Gen (Optional a, Optional a, Optional a)
genThrepleOptional = do
  a <- genOptional
  b <- genOptional
  c <- genOptional
  return $ (a,b,c)

type OptionalProduct = Optional (Product Int)

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
  describe "Optional (Monoid a) " $ do
    it "associativity " $ do
      forAll (genThrepleOptional ::  Gen (OptionalProduct, OptionalProduct ,OptionalProduct))
        (\(a,b,c)  ->  (a <> b) <> c == a <> (b <> c))
    it "associativity unitary" $ do
     ((Sum 1 <> Sum 2) <> Sum 3) `shouldBe` (Sum 1 <> (Sum 2 <> Sum 3))
    it "right identity" $ do
      forAll (genOptional :: Gen OptionalProduct ) (\a -> a <> mempty == a )
    it "left identity" $ do
      forAll (genOptional :: Gen OptionalProduct ) (\a -> mempty <> a == a )
