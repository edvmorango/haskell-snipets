module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

data Identity a = Identity a deriving (Eq, Show)

data Pair a b = Pair a b deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

pairGenIntChar :: Gen (Pair Int Char)
pairGenIntChar = pairGen

sumGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, return $ Second b]
  -- or
  -- elements [First a, Second b]
sumGenIntOrChar :: Gen (Sum Int Char)
sumGenIntOrChar = sumGen


instance Arbitrary Trivial where
  arbitrary = trivialGen

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen


-----


allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse



prop_EncodeDecode :: Property
prop_EncodeDecode =
  forAll charGen (\c -> ((charToMorse c) >>= morseToChar) == Just c )



main :: IO ()
main = quickCheck prop_EncodeDecode
