module SecondExercise where
  
  
import Data.List (elemIndex)
    
x :: Maybe Int
x = elemIndex 5 [1, 2, 3, 4, 5]

y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int 
max' = max

maxed :: Maybe Int 
maxed = Just (max') <*> x <*> y


xs = [1, 2, 3]
ys = [4, 5, 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y' :: Maybe Integer
y' = lookup 2 $ zip xs ys


-- I don't get it
summed :: Maybe Integer 
summed =  Just $ sum $ Just (+) <*> x' <*> y'

