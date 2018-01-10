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

instance (Show a) => Show (Identity a) where
  show (Identity a) = "Identity " ++ (show a)


identity :: Eq a => Identity a -> Identity a -> Bool
identity (Identity al) (Identity al') = al == al'


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =  ((a <> b) <>c) == (a <> (b <> c))

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentityType a =  Identity a -> Identity a -> Bool


genT :: (Arbitrary a) => Gen a
genT = do
  a <- arbitrary
  return a

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    b <- arbitrary
    return $ Identity b



tests :: IO ()
tests = hspec $ do
   describe "Semigroup" $ do
    it "associativity" $ do
      quickCheck (semigroupAssoc :: TrivAssoc)
    it "identity" $ do
      quickCheck (identity :: (IdentityType Int))
