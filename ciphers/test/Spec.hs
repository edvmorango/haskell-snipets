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

genShift :: Gen Int
genShift =  (genT :: Gen Int) `suchThat` (>0)

genCaesarTest :: Gen (String, Int)
genCaesarTest = do
  w <- genWord
  s <- genShift
  return (w,s)


caesarTest :: (String, Int) -> Bool
caesarTest (w, s) = w == decoded
  where encoded = C.encode s w
        decoded = C.decode s encoded


main :: IO ()
main = hspec $ do
  describe "Caesar cipher" $ do
    it "should encode and decode valid string" $ do
      forAll (genCaesarTest) caesarTest
  describe "Vinegere Cipher" $ do
    it "should encode valid string" $ do
      V.encode (V.makeShift "ALLY" ) "MEET AT DAWN" `shouldBe` "MPPR AE OYWY"
