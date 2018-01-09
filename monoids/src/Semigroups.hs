module Semigroups where

import Test.QuickCheck
import Test.Hspec
import Data.Semigroup

data Trivial  = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial


newtype Identity a = Identity a


instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Eq a => Eq (Identity a) where
  (Identity a) == (Identity a') = a == a'


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =  ((a <> b) <>c) == (a <> (b <> c))

identity :: (Eq a) => Identity a -> Identity a -> Bool
identity (Identity a ) (Identity a') =  a == a'



type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

instance Arbitrary Trivial where
  arbitrary = return Trivial

tests :: IO ()
tests = hspec $ do
   describe "Semigroup" $ do
    it "associativity" $ do
      quickCheck (semigroupAssoc :: TrivAssoc)
    -- it "identity" $ do
     -- forAll (genT :: Gen (Identity a, Identity a)) (\(Identity a, Identity b) -> a == b)
