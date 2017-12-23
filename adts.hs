{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Adts where

import Data.Int


data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Chevrolet | Ford | Tesla deriving (Eq, Show)

data Airline = Avianca | Latam | Varig deriving (Eq, Show)

data Size = Size Integer deriving (Eq, Show)

data Vehicle =  Car Manufacturer Price | Plane Airline Size
  deriving (Eq, Show)


myCar = Car Tesla (Price 200000)
urCar = Car Chevrolet (Price 50000)
ourCar = Car Ford (Price 100000)
bigJetPlane = Plane Avianca (Size 100)

--  For some reason this is exhaustive :(
-- isCar :: Vehicle -> Bool
-- isCar (Car _ _) = True
-- iscar _ = False

isCar :: Vehicle -> Bool
isCar v = case v of
  Car _ _ -> True
  _ -> False

isPlane :: Vehicle -> Bool
isPlane v = case v of
  Plane _ _ -> True
  _ -> False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Maybe Manufacturer
getManu v = case v of
  Car m _ ->  Just m
  _ -> Nothing



data Example = MakeExample Int deriving Show

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)

newtype IntInt = IntInt (Int, Int) deriving (Eq, Show)

instance TooMany (Int, String) where
  tooMany (_, n) = length n > 42

instance TooMany IntInt where
  tooMany (IntInt (a,b) ) = tooMany (a+b)

instance TooMany Goats where
  tooMany (Goats n) = tooMany n

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)


data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows   deriving (Eq, Show)

data ProgLang =
  Haskell
  | Agda
  | Idris
  | PureScript deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux
                      , OpenBSDPlusNevermindJustBSDStill
                      , Mac
                      , Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

data Programmer = Programmer {
                    os :: OperatingSystem
                  , lang :: ProgLang } deriving (Eq, Show)


allProgrammers :: [Programmer]
allProgrammers = [Programmer s l | s <- allOperatingSystems, l <- allLanguages]
