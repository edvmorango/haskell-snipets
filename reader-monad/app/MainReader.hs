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

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ f . ra -- or Reader $ (f . ra)

ask :: Reader a a
ask = Reader id
