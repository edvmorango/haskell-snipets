module Adhoc where

import Data.List

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

divideThenAdd :: Fractional a => a -> a -> a
divideThenAdd x y = (x / y) + 1

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
               | Woot deriving (Eq, Ord , Show)

settleDown x =
  if x == Woot then Blah
  else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String deriving (Eq, Ord , Show)
data Yeah = Yeah Bool deriving (Eq, Ord , Show)
data Papu = Papu Rocks Yeah deriving (Eq, Ord , Show)

truth = Papu (Rocks "chomskydoz") (Yeah True)
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus p p' = p > p'
comparePapus :: Papu -> Papu -> Bool

i :: RealFrac a => a
i = 1.0


freud :: Ord a => a -> a
freud x = x
-- b) freud :: Ord a => a -> a

myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

-- b) sigmund :: a -> a

-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)


young :: [Char] -> Char
young xs = head (sort xs)


mySort :: [Char] -> [Char]
mySort = sort

 -- signifier :: Ord a => [a] -> a
signifier :: [Char] -> Char
signifier xs = head (mySort xs)



chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aB a b = (aB a) == b


arith :: Num b => (a -> b) -> Integer -> a -> b
arith aB int a = (aB a) + (fromInteger int) --  fromInteger :: Integer -> a   a :: Num
