{-# LANGUAGE ViewPatterns #-}

module Functors where

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Hspec


data FixMePls a =  FixMe | Pls a deriving (Eq, Show)


instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)


data WhoCares a = DontCare | Matter a | DontKnow deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ DontCare = DontCare -- Change to DontKnow to break identity law
  fmap f (Matter m) = Matter (f m)
  fmap _ DontKnow = DontKnow

data CountingBad a = Acc Int a deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Acc n a) = Acc (n + 1) (f a)

data CountingNice a = Accum Int a deriving (Eq, Show)

instance Functor CountingNice where
  fmap f (Accum n s) = Accum n (f s)

  --- Heavy Lift

a = (+1) <$> read "[1]" :: [Int]

b =  (fmap . fmap) (++ " lol")  (Just ["Hi", "Hello"])

c =  fmap (*2) (\x -> x - 2)

d =  (fmap) ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (* 3) changed

-- Innermost

data Two a b = Two a b deriving (Eq, Show)

-- The First kind(a) should be part of functor structure
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

--------- TEsts

functorIdentity :: (Functor f, Eq (f a) ) => f a -> Bool
functorIdentity f = fmap (id) f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =  ( fmap g (fmap f x) ) == ( fmap (g.f) x )

functorCompose' :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

--

newtype Wrap a = Wrap a deriving (Eq, Show)

instance Functor Wrap where
  fmap f (Wrap a) = Wrap (f a)

instance (Arbitrary a) => Arbitrary (Wrap a) where
  arbitrary = do
    a <- arbitrary
    return $ Wrap a

type FunctorIdentityType a = Wrap a -> Bool
type FunctorIdentityCompositionType' a = Fun a a -> Fun a a -> Wrap a -> Bool
type FunctorIdentityCompositionType a b = Fun a b -> Fun b a -> Wrap a -> Bool

---

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance (Arbitrary a, Eq a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return $ Pair a a

type PairType a = Pair a -> Bool
type PairCompositionType' a = Fun a a -> Fun a a -> Pair a  -> Bool
type PairCompositionType a b = Fun a b -> Fun b a -> Pair a -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoType a b = Two a b -> Bool
-- type TwoCompositionType' a c = Fun a a -> Fun a a -> Two a c -> Bool
type TwoCompositionType a b c = Fun b c -> Fun b c -> Two a b -> Bool



-- data Two a b = Two a b
data Three a b c = Three a b c deriving (Eq, Show)
data Three' a b = Three' a b b deriving (Eq, Show)
data Four a b c d = Four a b c d deriving (Eq, Show)
data Four' a b = Four' a a a b deriving (Eq, Show)
data Trivial = Trivial deriving (Eq, Show)


tests :: IO()
tests = hspec $ do
  describe "Functors" $ do
    it "f a(Int) identity" $ do
      quickCheck (functorIdentity :: FunctorIdentityType Int )
    it "f a -> g a composition " $ do
      quickCheck (functorCompose' :: FunctorIdentityCompositionType Int Float)
    it "Pair identity" $ do
      quickCheck (functorIdentity :: PairType Int )
    it "Pair composition " $ do
      quickCheck (functorCompose' :: PairCompositionType Int Float)
    it "Two identity" $ do
      quickCheck (functorIdentity :: TwoType Int String )
    it "Two composition " $ do
      quickCheck (functorCompose' :: TwoCompositionType Int String String)
    it "Three identity" $ do
      quickCheck (functorIdentity :: FunctorIdentityType Int )
    it "Three composition " $ do
      quickCheck (functorCompose' :: FunctorIdentityCompositionType Int Float)
    it "Three' identity" $ do
      quickCheck (functorIdentity :: FunctorIdentityType Int )
    it "Three' composition " $ do
      quickCheck (functorCompose' :: FunctorIdentityCompositionType Int Float)
    it "Four identity" $ do
      quickCheck (functorIdentity :: FunctorIdentityType Int )
    it "Four composition " $ do
      quickCheck (functorCompose' :: FunctorIdentityCompositionType Int Float)
    it "Four' identity" $ do
      quickCheck (functorIdentity :: FunctorIdentityType Int )
    it "Four' composition " $ do
      quickCheck (functorCompose' :: FunctorIdentityCompositionType Int Float)






--
