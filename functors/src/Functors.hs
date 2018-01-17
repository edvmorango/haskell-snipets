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


tests :: IO()
tests = hspec $ do
  describe "Functors" $ do
    it "f a(Int) identity" $ do
      quickCheck (functorIdentity :: FunctorIdentityType Int )
    it "f a -> g a composition " $ do
      quickCheck (functorCompose' :: FunctorIdentityCompositionType Int Float)






--
