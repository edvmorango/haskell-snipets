module Foldrs where

import Data.Monoid

data Identity a = Identity a deriving (Eq, Show)

instance Foldable Identity where
  foldr f acc (Identity c) = f c acc
  foldl f acc (Identity c) = f acc c
  foldMap f (Identity c) = f c
  
ex1 = foldr (*) 1 (Identity 10)

type IntProduct = Product Int

fmEx1 = foldMap (*5) (Identity 5) :: IntProduct


newtype Min a = Min {getMin :: Maybe a} deriving (Eq, Show)

instance (Ord a) => Monoid (Min a) where
  mempty = (Min Nothing)
  mappend (Min Nothing) a = a
  mappend a (Min Nothing)  = a
  mappend (Min a) (Min a') = Min (min a a')

newtype Max a = Max {getMax :: Maybe a} deriving (Eq, Show)

instance (Ord a) => Monoid (Max a) where
  mempty = (Max Nothing)
  mappend (Max Nothing) a = a
  mappend a (Max Nothing)  = a
  mappend (Max a) (Max a') = Max (max a a')

mSum :: (Foldable t, Num a) => t a -> a
mSum v = getSum $ foldMap (Sum) v 

mProduct :: (Foldable t, Num a) => t a -> a
mProduct v = getProduct $ foldMap (Product) v 

mElem :: (Foldable t, Eq a) => a -> t a -> Bool
mElem e w = getAny $ foldMap (Any . ((==) e)) w

mMinimum :: (Foldable t, Ord a) => t a -> Maybe a
mMinimum m = getMin $ foldMap (Min . Just) m

mMaximum :: (Foldable t, Ord a) => t a -> Maybe a
mMaximum m = getMax $ foldMap (Max . Just) m


-- Any value ocurrence will falsify it ( defined into foldable instance)
mNull :: (Foldable t) =>  t a -> Bool
mNull = foldr (\_ _ -> False) True

mLength :: (Foldable t) => t a -> Int
mLength = foldr (\_ acc -> acc + 1) 0

mToList :: (Foldable t) => t a -> [a]
mToList = foldr (\c acc ->  c : acc) []


myFold :: (Foldable t, Monoid m) => t m -> m
myFold = foldMap id

myFold' :: (Foldable t, Monoid m) => t m -> m
myFold' = foldr (<>) (mempty)

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap f v = foldr (\c acc ->  (f c) <> acc) (mempty) v
  
myFoldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap' f v = foldr (mappend . f) (mempty) v





