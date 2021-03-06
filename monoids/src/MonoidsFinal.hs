{-# LANGUAGE DeriveGeneric #-}

module MonoidsFinal where

import Test.QuickCheck
import Test.Hspec
import Data.Monoid
import GHC.Generics
import qualified Data.Semigroup as S


semigroupAssoc :: (Eq a, Monoid a) => a -> a -> a -> Bool
semigroupAssoc a b c =  a <> (b <> c) == (a <> b) <> c

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentity a = (mempty <> a) == a



-----------------------------------------

data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance S.Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
   mempty = Trivial
   mappend = (S.<>)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-----------------------------------------

data Identity a = Identity a deriving (Eq, Show)

instance (Arbitrary a, S.Semigroup a, Monoid a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance (S.Semigroup a) => S.Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity ( a S.<> a')

instance (Monoid a, S.Semigroup a) => Monoid (Identity a) where
   mempty = (Identity mempty)
   mappend = (S.<>)

type IdentityAssoc a = (Identity a) -> (Identity a) -> (Identity a) -> Bool


-----------------------------------------
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a
        , Arbitrary b
        , S.Semigroup a
        , S.Semigroup b
        , Monoid a
        , Monoid b) => Arbitrary (Two a b) where
   arbitrary = do
     a <- arbitrary
     b <- arbitrary
     return $ Two a b


instance (S.Semigroup a, S.Semigroup b) => S.Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a S.<> a') (b S.<> b')

instance (Monoid a, S.Semigroup a, Monoid b,  S.Semigroup b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (S.<>)

type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool

-------------------------------------------

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary (BoolConj) where
  arbitrary = do
    a <- arbitrary
    return $ (BoolConj a)

instance  S.Semigroup (BoolConj) where
  (BoolConj a) <> (BoolConj a') = BoolConj (a && a')

instance Monoid (BoolConj) where
  mempty = BoolConj (True)
  mappend = (S.<>)

type BoolConjType = BoolConj -> BoolConj -> BoolConj -> Bool

-------------------------------------------
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary (BoolDisj) where
  arbitrary = do
    a <- arbitrary
    return $ (BoolDisj a)

instance  S.Semigroup (BoolDisj) where
  (BoolDisj a) <> (BoolDisj a') = BoolDisj (a || a')

instance Monoid (BoolDisj) where
  mempty = BoolDisj (False)
  mappend = (S.<>)

type BoolDisjType = BoolDisj -> BoolDisj -> BoolDisj -> Bool
-------------------------------------------

genFunc :: (CoArbitrary a, Arbitrary b) => Gen (a -> b)
genFunc = arbitrary

-------------------------------------------
newtype Combine a b = Combine { unCombine :: (a -> b) }

genCoCombine :: (CoArbitrary a, Arbitrary b) => Gen (Combine a b)
genCoCombine = do
  f <- genFunc
  return $ Combine f

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = genCoCombine

instance (S.Semigroup b) => S.Semigroup (Combine a b) where
  (Combine b) <> (Combine b') = Combine ( b S.<> b')

instance (S.Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine (mempty)
  mappend = (S.<>)


type CombineType a b = (Combine a b) -> (Combine a b) -> (Combine a b) -> Bool

-------------------------------------------
newtype Comp a = Comp (a -> a)

genFunA :: (CoArbitrary a, Arbitrary a) => Gen (a -> a)
genFunA = arbitrary

genCoComp :: (CoArbitrary a, Arbitrary a ) => Gen (Comp a)
genCoComp = do
  f <- genFunA
  return $ Comp f

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = genCoComp

instance (S.Semigroup a) => S.Semigroup (Comp a) where
  (Comp a) <> (Comp a') = Comp ( a S.<> a')

instance (S.Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty = Comp (mempty)
  mappend = (S.<>)


type CompType a b = (Comp a) -> (Comp a ) -> (Comp a ) -> Bool


-------------------------------------------
-------------------------------------------
-------------------------------------------
-------------------------------------------
-------------------------------------------

tests :: IO ()
tests = hspec $ do
  describe "Trivial" $ do
    it "Trivial assoc" $ do
      quickCheck (semigroupAssoc :: TrivAssoc)
    it "Trivial rid" $ do
      quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    it "Trivial lid" $ do
      quickCheck (monoidRightIdentity :: Trivial -> Bool)
  describe "Identity" $ do
    it "Identity assoc" $ do
      quickCheck (semigroupAssoc :: (IdentityAssoc String))
    it "Identity rid" $ do
      quickCheck (monoidLeftIdentity :: (Identity (Sum Int)) -> Bool)
    it "Identity lid" $ do
      quickCheck (monoidRightIdentity :: (Identity (Sum Int))  -> Bool)
  describe "Two" $ do
    it "Two assoc" $ do
      quickCheck (semigroupAssoc :: (TwoAssoc String (Sum Int) ))
    it "Two rid" $ do
      quickCheck (monoidLeftIdentity :: (Two String (Sum Int)) -> Bool)
    it "Two lid" $ do
      quickCheck (monoidRightIdentity :: (Two String (Sum Int))  -> Bool)
  describe "BoolConj" $ do
    it "BoolConj assoc" $ do
      quickCheck (semigroupAssoc :: (BoolConjType))
    it "BoolConj rid" $ do
      quickCheck (monoidLeftIdentity :: (BoolConj -> Bool))
    it "BoolConj lid" $ do
      quickCheck (monoidRightIdentity :: (BoolConj -> Bool))
  describe "BoolDisj" $ do
    it "BoolDisj assoc" $ do
      quickCheck (semigroupAssoc :: (BoolDisjType))
    it "BoolDisj rid" $ do
      quickCheck (monoidLeftIdentity :: (BoolDisj -> Bool))
    it "BoolDisj lid" $ do
      quickCheck (monoidRightIdentity :: (BoolDisj -> Bool))
  -- describe "Combine" $ do
    -- it "Combine assoc" $ do
      -- quickCheck (semigroupAssoc :: (CombineType String String))
    -- it "Combine rid" $ do
      -- quickCheck (monoidLeftIdentity :: (Combine (Sum Int) String -> Bool))
    -- it "Combine lid" $ do
      -- quickCheck (monoidRightIdentity :: (Combine (Sum Int) String -> Bool))
