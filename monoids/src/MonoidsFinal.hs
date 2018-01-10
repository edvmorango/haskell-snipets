module MonoidsFinal where

import Test.QuickCheck
import Test.Hspec
import Data.Monoid
import qualified Data.Semigroup as S



  -----------------------------------------

data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance S.Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
   mempty = Trivial
   mappend = (S.<>)

semigroupAssoc :: (Eq a, Monoid a) => a -> a -> a -> Bool
semigroupAssoc a b c =  a <> (b <> c) == (a <> b) <> c

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentity a = (mempty <> a) == a

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool


tests :: IO ()
tests = hspec $ do
  describe "Trivial" $ do
    it "Trivial assoc" $ do
      quickCheck (sa :: TrivAssoc)
    it "Trivial rid" $ do
      quickCheck (mli :: Trivial -> Bool)
    it "Trivial lid" $ do
      quickCheck (mlr :: Trivial -> Bool)
  where sa = semigroupAssoc
        mli = monoidLeftIdentity
        mlr = monoidRightIdentity
