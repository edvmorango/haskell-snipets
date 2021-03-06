{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Functors where

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Hspec
import GHC.Arr

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
type TwoCompositionType a b c = Fun b c -> Fun b c -> Two a b -> Bool

----

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeType a b c = Three a b c -> Bool
type ThreeCompositionType a b c d = Fun c d -> Fun c d -> Three a b c -> Bool



data Three' a b = Three' a b b deriving (Eq, Show)


-- Functors are about lift the last type (more to right), not just the last param
instance Functor (Three' a ) where
  fmap f (Three' a b b') = Three' a  (f b) (f b')

instance (Arbitrary a, Arbitrary b ) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Three' a b b

type Three'Type a b  = Three' a b  -> Bool
type Three'CompositionType a b c = Fun b c -> Fun b c -> Three' a b -> Bool

--

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourType a b c d = Four a b c d -> Bool
type FourCompositionType a b c d e = Fun  d e -> Fun d e -> Four a b c d -> Bool

--

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

type Four'Type a b = Four' a b  -> Bool
type Four'CompositionType a b c = Fun b c -> Fun b c -> Four' a b -> Bool

--



-- Possibly

data Option a = None | Some a deriving (Eq,Show)

instance Functor Option where
  fmap f (Some a) = Some (f a)
  fmap _ _ = None

instance (Arbitrary a) => Arbitrary (Option a) where
  arbitrary = do
    a <- arbitrary
    return $ Some (a)

data Try a b = Fail a | Suc b deriving (Eq, Show)

instance Functor (Try a) where
  fmap f (Fail a) = Fail a
  fmap f (Suc b) = Suc (f b)


applyTry :: (s -> b) -> (Try f s) -> (Try f b)
applyTry f fa =  f <$> fa

--- CHapter exercises



data FollowedBool a = FTrue a | FFalse a deriving (Eq, Show)


instance Functor (FollowedBool) where
  fmap f (FTrue a) = FTrue (f a)
  fmap f (FFalse b) = FFalse (f b)


data OptionalBool a = OTrue a | OFalse deriving (Eq, Show)

instance Functor (OptionalBool) where
  fmap _ (OFalse) = OFalse
  fmap f (OTrue a) = OTrue (f a)


-- don't work, and I not found any solution 
newtype Mu f = InF { outF :: f (Mu f)}  

-- Rearrange
data Sum a b = First a | Second b

instance Functor (Sum e) where 
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)


data Company a b c = DeepBlue a c | Something b


instance Functor (Company e e') where 
  fmap f (DeepBlue a c) = DeepBlue (a) (f c)
  fmap _ (Something b) = Something b

data More a b = L a b a | R b a b deriving (Eq, Show)

-- functors transforms just one type, 
-- the hidden, X -> A, so B will be modified
-- so, just the last parameter is `functable`
-- without the flip using.
instance Functor (More x) where
  fmap f (L a b a') = L a (f b) a' 
  fmap f (R b a b') = R (f b) a (f b')

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a

data K a b = K a

instance Functor (K x) where
  fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a) ) = Flip $ K (f a)



data EvilGoateeConst a b = GoatyConst b

--Should use the most generic ADT (phantom type ?)
instance Functor (EvilGoateeConst x) where
  fmap f (GoatyConst a) = GoatyConst (f a)




data LiftItOut f a = LiftItOut (f a)

instance (Functor fu) => Functor (LiftItOut fu) where 
  fmap f (LiftItOut a) = LiftItOut (fmap f a)

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa a b) = DaWrappa (fmap f a) (fmap f b)

data IgnoreOne f g a b =  IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething a b) = IgnoringSomething (a) (fmap f b)


data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious o a t) = Notorious (o) (a) (fmap f t)

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor (List) where
  fmap _ Nil = Nil
  fmap f (Cons c (a)) = Cons (f c) (fmap f a)  



data GoatLord a =
    NoGoat 
  | OneGoat a 
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor (GoatLord) where
  fmap f (NoGoat) = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c) 


data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor (TalkToMe) where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a) 
  fmap f (Read l) = Read (fmap f l)



getInt :: IO Int
getInt = fmap read getLine

type Nat f g = forall a . f a -> g a

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
      quickCheck (functorIdentity :: ThreeType Int Int Int)
    it "Three composition " $ do
      quickCheck (functorCompose' :: ThreeCompositionType Int Int Int Int)
    it "Three' identity" $ do
      quickCheck (functorIdentity :: Three'Type Int Int )
    it "Three' composition " $ do
      quickCheck (functorCompose' :: Three'CompositionType Int String String)
    it "Four identity" $ do
      quickCheck (functorIdentity :: FourType Int Int Double Int )
    it "Four composition " $ do
      quickCheck (functorCompose' :: FourCompositionType Int Int Int String String)
    it "Four' identity" $ do
      quickCheck (functorIdentity :: Four'Type Int Float )
    it "Four' composition " $ do
      quickCheck (functorCompose' :: Four'CompositionType Int Float Float )


--
