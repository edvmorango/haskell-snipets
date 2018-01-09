module Monoids where

import Data.Monoid

listAppend = mappend [1,2,3] [4,5,6]
listConcat = mconcat [[1..3],[1..6],[1..9]]
listAppendOp = [1,2,3] <> [4,5,6] <> [7..9] -- ...

sumMonoid = (Sum 1) <> (Sum 2)
getSumRsult = getSum $ sumMonoid

productMonoid = getProduct $ (Product 10) <> (Product 5)

withIdentity = getSum $ mempty <> (Sum 10) <> (Sum 15)

foldrMonoidal = foldr (<>) mempty [Sum 10, Sum 20, Sum 1, Sum 19]


data Booly a = Fal | Tru deriving (Eq, Show)

instance Monoid (Booly a) where
  mappend Fal _ = Fal
  mappend _ Fal = Fal
  mappend Tru Tru = Tru

data Option a = Nada | Algo a deriving (Eq, Show)

instance Monoid a => Monoid (Option a) where
  mappend Nada (Algo a) = Algo a
  mappend (Algo a) Nada = Algo a
  mappend (Algo a) (Algo b) = Algo $ a <> b

  mempty = Nada
