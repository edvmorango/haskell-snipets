module Main where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "helloo!"


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

addition :: Int -> Int -> Int
addition a b = a + b

recursiveProduct :: (Eq a, Num a) => a -> a -> a
recursiveProduct a b
  | b == 1 = a
  | otherwise = a + (recursiveProduct a (b-1))



trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genTuple3 :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genTuple3 = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

callTupleCharInt :: IO ()
callTupleCharInt = sample (genTuple :: Gen (Char, Int)) -- Setting Higher kinded types

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing), (3, return (Just a))]



main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 should be greater than 1" $ do
      (addition 1 1) > 1 `shouldBe` True
    it "2 + 2 should be 4" $ do
      (addition 2 2) `shouldBe` 4
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
  describe "DividedBy" $ do
    it "3 divided by 3 should be (1,0)" $ do
      (dividedBy 3 3) `shouldBe` (1,0)
    it "5 divided by 2 should be (2,1)" $ do
      (dividedBy 5 2) `shouldBe` (2,1)
  describe "RecursiveProduct" $ do
    it "recursiveProduct  of 5 and 3 should be 15" $ do
      (recursiveProduct 5 3) `shouldBe` 15
