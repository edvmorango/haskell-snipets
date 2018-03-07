module Applicatives where

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Hspec


-- Generators

genT :: (Arbitrary a, Eq a) => Gen a
genT = do
  a <- arbitrary
  return a

genList :: (Arbitrary a, Eq a) => Gen a -> Gen [a]
genList a = do 
  as <- listOf a 
  return as


tests :: IO()
tests = hspec $ do
  describe "Functors" $ do
    it "id, fmapId and pureId are the same" $ do 
      forAll  (genList (genT :: Gen [Int])) 
        (\a -> let sid = id a  
                   fid = id <$> a
                   pid = (pure id) <*> a 
               in sid == fid && fid == pid )
  
    -- it "fmap id" $ do
      -- forAll (genList (genT :: Gen [Int])) (\a ->  (id <$> a) == a )
    
  --  quickCheck (functorIdentity :: FunctorIdentityType Int )
