module Main where

import Data.Char
import Lib

main :: IO ()
main = someFunc

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev r = reverse r

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped f = (rev <$> cap) f

tupled :: [Char] -> ([Char], [Char])
tupled cs = ((,) <$> cap <*> rev) cs

newtype Reader r a = Reader
  { runReader :: r -> a
  }

-- instance Functor (Reader r) where
--  fmap :: (a -> b) -> Reader r a -> Reader r b
--  fmap f (Reader ra) = Reader $ (f . ra) -- or Reader $ (f . ra)
ask :: Reader a a
ask = Reader id

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers = Person (HumanName "Bill") (DogName "Ether") (Address "Street 1")

pers2 = Person (HumanName "Gill") (DogName "Serr") (Address "Street 2")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b
