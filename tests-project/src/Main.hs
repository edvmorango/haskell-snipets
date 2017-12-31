module Main where

import Test.Hspec


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

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 should be greater than 1" $ do
      (addition 1 1) > 1 `shouldBe` True
    it "2 + 2 should be 4" $ do
      (addition 2 2) `shouldBe` 4
  describe "DividedBy" $ do
    it "3 divided by 3 should be (1,0)" $ do
      (dividedBy 3 3) `shouldBe` (1,0)
    it "5 divided by 2 should be (2,1)" $ do
      (dividedBy 5 2) `shouldBe` (2,1)
  describe "RecursiveProduct" $ do
    it "recursiveProduct  of 5 and 3 should be 15" $ do
      (recursiveProduct 5 3) `shouldBe` 15
