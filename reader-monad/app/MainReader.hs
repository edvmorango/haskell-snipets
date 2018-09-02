{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Monad (join)
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

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ (f . ra) -- or Reader $ (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader (\fix -> a)
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader f) (Reader a) = (Reader (\input -> (f input) (a input)))

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader v) f = join $ Reader (\r -> f (v r))

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

-- (a -> b) -> f a -> f b
-- (Name -> (Address -> Dog)) -> (-> Person) Name -> (-> Person) (Address -> Dog)
-- (Name -> (Address -> Dog)) -> (Person -> Name) -> (Person -> (Address -> Dog)) 
a = Dog <$> dogName

-- f (a -> b) -> f a -> f b
-- ((-> Person) (Address -> Dog)) -> (-> Person) Address) -> ((-> Person) Dog)
-- (Person -> (Address -> Dog) ->  (Person -> Address) -> (Person -> Dog)
aa = Dog <$> dogName <*> address
