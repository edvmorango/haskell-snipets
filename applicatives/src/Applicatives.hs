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


type FCompose = [Int] -> Fun Int Int -> Fun Int Int -> Bool

funCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
funCompose fu (Fun _ f)  (Fun _ g) = (fmap (g . f) fu) == ( (fmap g . fmap f) fu) 

type ACompose = Maybe Int ->  (Fun  Int Int) -> Maybe (Fun Int Int) -> Bool

apCompose :: (Eq (f c), Applicative f) => f a ->  (Fun a b ) -> (Fun b c) -> Bool
apCompose v  (Fun _ f)  (Fun _ g) = ((pure (.) <*> (pure g) <*> (pure f) <*> v )) == ((pure g) <*> ( (pure f) <*> v))

tests :: IO()
tests = hspec $ do
  describe "Functors" $ do
    it "(Identity) -  id, fmapId and pureId are the same" $ do 
      forAll  (genList (genT :: Gen [Int])) 
        (\a -> let sid = id a  
                   fid = id <$> a
                   pid = (pure id) <*> a 
               in sid == fid && fid == pid )
    -- it "(Composition)" $ do
      -- forAll (genList ::   )