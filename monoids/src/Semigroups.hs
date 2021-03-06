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

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    b <- arbitrary
    return $ Identity b

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three a' b' c')  = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c


data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (Four a b c d) <> (Four a' b' c' d')  = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool
type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool
type FourAssoc a b c d= Four a b c d -> Four a b c d -> Four a b c d-> Bool

---------------------


newtype BoolConj = BoolConj Bool  deriving (Eq, Show)

instance Semigroup (BoolConj) where
  (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup (BoolDisj) where
  (BoolDisj a) <> (BoolDisj b) = BoolDisj (a || b)

----------------------

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst a) <> (Fst b) = Fst b
  (Fst a) <> (Snd b) = Snd b
  (Snd a) <> _ = Snd a


----------------------

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where -- The result will always b, so just Combine it
    Combine b <> Combine b' = Combine (b <> b')

fCombine = Combine $ \n -> Sum (n + 1)
gCombine = Combine $ \n -> Sum (n - 1)

--------------------------

newtype Comp a = Comp { unComp :: (a -> a) }

instance (Semigroup a) => Semigroup (Comp a) where
  (Comp a) <> (Comp a') = Comp (a <> a')


------------------------
data Validation a b = Fail a | Suc b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
    (Suc b) <> _ = (Suc b)
    (Fail a) <> (Fail a') = Fail ( a <> a')
    (Suc b) <> (Suc _) = Suc b
    _ <> (Suc b) = (Suc b)

------------------------

tests :: IO ()
tests = hspec $ do
   describe "Semigroup" $ do
    it "associativity" $ do
      quickCheck (semigroupAssoc :: TrivAssoc)
    it "identity" $ do
      quickCheck (identity :: (IdentityType Int))
    it "associativity two" $ do
      quickCheck (semigroupAssoc :: (TwoAssoc (Sum Integer) String))
    it "associativity three" $ do
      quickCheck (semigroupAssoc :: (ThreeAssoc (Sum Integer) String String))
    it "associativity four" $ do
      quickCheck (semigroupAssoc :: (FourAssoc String String (Product Integer) String))
    it "bool conj " $ do
      ((BoolConj True) <> (BoolConj False)) `shouldBe` (BoolConj False)
    it "bool conj 2" $ do
      ((BoolConj True) <> (BoolConj True)) `shouldBe` (BoolConj True)
    it "bool disj 1" $ do
      ((BoolDisj True) <> (BoolDisj False)) `shouldBe` (BoolDisj True)
    it "bool disj 2" $ do
      ((BoolDisj False) <> (BoolDisj False)) `shouldBe` (BoolDisj False)
    it "or 1" $ do
      ((Fst 10) <> (Snd 20)) `shouldBe` (Snd 20)
    it "or 2" $ do
      ((((Fst 10) :: Or Int Int)) <> ((Fst 20) :: Or Int Int)) `shouldBe` (((Fst 20) :: Or Int Int))
    it "or 3" $ do
      ((Snd 10) <> (Fst 20)) `shouldBe` (Snd 10)
    it "or 4" $ do
      ((((Snd 10) :: Or Int Int)) <> ((Snd 20) :: Or Int Int)) `shouldBe` (((Snd 10) :: Or Int Int))
    it "combine 1" $ do
      (unCombine (fCombine <> gCombine)) 0 `shouldBe` 0
    it "combine 2" $ do
      (unCombine (fCombine <> gCombine)) 1 `shouldBe` 2
    it "combine 3" $ do
      (unCombine (fCombine <> fCombine)) 1 `shouldBe` 4
    it "combine 4" $ do
      (unCombine (gCombine <> fCombine)) 1 `shouldBe` 2
    it "validation 1" $ do
      (((Fail "fail") :: Validation String (Sum Int)) <> ((Fail "fish") :: Validation String (Sum Int))) `shouldBe` (Fail "failfish")
    it "validation 2" $ do
      (Suc (Sum 10)) <> (Fail "fish") `shouldBe` (Suc (Sum 10))
    it "validation 3" $ do
      (Fail "fail") <> (Suc (Sum 10)) `shouldBe` (Suc (Sum 10))
    it "validation 4" $ do
      ( (Suc (Sum 10) :: Validation String (Sum Int))  <> (Suc (Sum 20) :: Validation String (Sum Int)) ) `shouldBe` (Suc (Sum {getSum = 10}))
