module Adhoc where



data TisAnInteger a = TisAn a

instance (Integral a) => Eq (TisAnInteger a)  where
  (==) (TisAn a) (TisAn a') =  a == a'

data TwoIntegers a = Two a a

instance (Integral a) => Eq (TwoIntegers a)  where
  (Two a1 a2) == (Two a1' a2') =  a1 == a1' && a2 == a2'

data TStringOrInt = TString String | TInt Int

instance Eq TStringOrInt where
  (==) (TString a) (TString a') = a == a'
  (==) (TInt a) (TInt a') = a == a'
  (==) _ _ = False


data Pair a = Pair a a

instance (Eq a) => Eq (Pair a) where
  (==) (Pair a1 a2) (Pair a1' a2') = a1 == a1' && a2 == a2'

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a = ThisOne a | ThatOne a

instance (Eq a) => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False
