module Functors where

import Test.QuickCheck
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

newtype Wrap a = Wrap a deriving (Eq, Show)

instance Functor Wrap where
  fmap f (Wrap a) = Wrap (f a)

instance (Arbitrary a, Eq a) => Arbitrary (Wrap a) where
  arbitrary = do
    a <- arbitrary
    return $ Wrap a

functorIdentity :: (Functor f, Eq (f a) ) => f a -> Bool
functorIdentity f = fmap (id) f == f

type FunctorIdentityType a = Wrap a -> Bool

tests :: IO()
tests = hspec $ do
  describe "Functors" $ do
    it "f a(Int) identity" $ do
      quickCheck (functorIdentity :: FunctorIdentityType Int )







--
