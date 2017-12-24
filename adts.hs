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



data Tree a = Leaf | Node (Tree a) a (Tree a)
      deriving (Eq, Ord, Show)


insert :: (Ord a) =>  a -> Tree a -> Tree a
insert a Leaf = Node Leaf a Leaf
insert a (Node left c right  )
  | a == c = Node left a right
  | a < c =  Node ( insert a left) c right
  | a > c =  Node left c (insert a right)


mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)


testTree' :: Tree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if mapTree (+1) testTree' == mapExpected
          then print "yup okay!"
          else error "test failed!"


testTreeList = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

preorder :: Tree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++  (preorder left) ++ (preorder right)

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (preorder left) ++ [a] ++ (preorder right)

postorder :: Tree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (preorder left) ++ (preorder right) ++ [a]


testPreorder :: IO ()
testPreorder = if preorder testTreeList == [2, 1, 3]
               then putStrLn "Preorder fine!"
               else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inorder testTreeList == [1, 2, 3]
              then putStrLn "Inorder fine!"
              else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder = if postorder testTreeList == [1, 3, 2]
                then putStrLn "Postorder fine!"
                else putStrLn "postorder failed check"

testTreeMain :: IO ()
testTreeMain = do
  testPreorder
  testInorder
  testPostorder

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) = foldTree f leftF right
    where cur = f a acc
          leftF = foldTree f cur left
