module Monads where

import Control.Monad (join)
import Control.Applicative

someFunc :: IO ()
someFunc = putStrLn "someFunc"



--  fmap (\x -> return (x * 1 )
--  fmap (a -> m a) [1,2,3] =  [[1], [2], [3]]
--  join [[1], [2], [3]] = [1,2,3]

bind :: (Monad m) => (a -> m a) -> m a -> m a
bind f m = join $ fmap f m


-- executes value after value, 
-- generates a layered list 
-- [ 1 ,2,3] -> [[]]
-- [ 2 ,3 ] -> [[], [4,4]]
-- [ 3 ] -> [[] , [4,4], []]
-- flatten = [4,4]
twiceWhenEven :: [Integer] -> [Integer] 
twiceWhenEven xs = do
   x <- xs
   if (even x) then [x*x, x*x] else []
   

data Person = Person { name :: String
                      ,age :: Int
                      ,salary :: Double} deriving (Eq, Show)
   
noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = return s

noNegative :: Int -> Maybe Int
noNegative v
  | v < 0 = Nothing
  | otherwise = return v 

minimumSalary :: Double -> Maybe Double
minimumSalary s 
  | s < 900 = Nothing
  | otherwise = return s
  
  
-- In Terms of applicative

mkPersonAp1 :: String -> Int -> Double -> Maybe Person
mkPersonAp1 n a s = pure (Person) <*> (noEmpty n) <*> (noNegative a) <*> (minimumSalary s)    

mkPersonAp2 :: String -> Int -> Double -> Maybe Person
mkPersonAp2 n a s = fmap (Person) (noEmpty n) <*> (noNegative a) <*> (minimumSalary s)    

mkPersonAp3 :: String -> Int -> Double -> Maybe Person
mkPersonAp3 n a s = liftA3 (Person) (noEmpty n) (noNegative a) (minimumSalary s)    
  
-- In terms of Monad

mkPerson :: String -> Int -> Double -> Maybe Person
mkPerson n a s = do 
  name <- noEmpty n
  age <- noNegative a
  sal <- minimumSalary s
  return $ Person name age sal

mkPerson2 ::  String -> Int -> Double -> Maybe Person
mkPerson2 n a s = noEmpty n >>= (\name ->  noNegative a >>= (\age ->   minimumSalary s >>= (\sal -> return (Person name age sal)) ))
  
  
-- This computations are independents before pure?  
-- mkPersonAPM :: String -> Int -> Double -> Maybe Person
mkPersonAPM n a s = do 
  name <- noEmpty n
  age <- noNegative a
  sal <- minimumSalary s
  pure (name, age, sal)





