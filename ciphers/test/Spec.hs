module Spec where

import Test.QuickCheck
import Test.Hspec
import qualified Cipher as C
import qualified VinegereCipher as V

genT :: (Arbitrary a, Eq a, Ord a) => Gen a
genT = do
  a <- arbitrary
  return a

genChar :: Gen Char
genChar = elements $ concat [['a'..'z'],['A'..'Z'],[' ']]

genWord :: Gen String
genWord = listOf genChar

main :: IO ()
main = hspec $ do
  describe "Caesar cipher" $ do
    it "should encode valid string" $ do
      True
    it "should decode valid string" $ do
      True
  describe "Vinegere Cipher" $ do
    it "should encode valid string" $ do
      True
    it "shoud decode valid string" $ do
      True
