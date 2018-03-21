module Monads where

import Control.Monad (join, (>=>))
import Control.Applicative
import Test.Hspec  
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes
import Test.QuickCheck

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


-- This computations are independents, a applicative use case
-- mkPersonAPM :: String -> Int -> Double -> Maybe Person
mkPersonAP n a s = do 
  name <- noEmpty n
  age <- noNegative a
  sal <- minimumSalary s
  pure (name, age, sal)

-- Either Monad

type Founded = Int
type Coders = Int

data SoftwareShop = Shop {
   founded :: Founded
  ,programmers :: Coders
} deriving (Eq, Show)

data FoundedError = 
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded y 
  | y < 0 = Left $ NegativeYears y
  | y > 50 = Left $ TooManyYears y
  | otherwise = Right y

validateCoders :: Int -> Either FoundedError Coders
validateCoders  c
  | c < 0 = Left $ NegativeCoders c
  | c > 50 = Left $ TooManyCoders c
  | otherwise = Right c

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop 
mkSoftware years coders = do
  y <- validateFounded years
  c <- validateCoders coders
  if ( c > y `div` 10 )
    then Left (TooManyCodersForYears y c)
    else return $ Shop y c

-- Either Monad Impl

data Sum a b = First a | Second b  deriving (Eq, Show) 

instance (Monoid b, Monoid a)  => Monoid (Sum a b) where
  mempty = Second (mempty)
  mappend (Second b) (Second b') = Second (mappend b b')
  mappend (First a) (First a') = First (mappend a  a')
  mappend (First a) _ = First (a)
  mappend _ (First a) = First (a)


instance Functor (Sum a ) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure b = Second b
  (<*>) (First a) _ = First a
  (<*>) _ (First a) = First a
  (<*>) (Second f) (Second a) = pure (f a)

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = First a
  (>>=) (Second b) (f) = f b
  

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
      a <- arbitrary
      b <- arbitrary
      elements [ First a, Second b ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq


-- Composition

sayHi :: String -> IO String
sayHi g = do
  putStrLn g
  getLine

readM :: Read a => String -> IO a
readM = return . read  

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "How old are you?"

askForAgeExplicit :: IO Int
askForAgeExplicit =
   sayHi "How old are you?" >>= readM :: IO Int 
   --   g ∘ f
   --      f             g
   --  (a -> m a) -> (a -> m b)
   --  (String -> IO String) -> (String -> IO Int)


type CheckerSum =  Sum (String, String, String) (String, String, String)

tests :: IO()
tests = hspec $ do
  describe "Laws" $ do
    it "Monoid" $ do
      quickBatch $ monoid (First ("Success")  :: Sum String String )
    it "Functor" $ do
      quickBatch $ functor (First ("C1", "C2", "C3") :: CheckerSum )   
    it "Applicative" $ do
      quickBatch $ applicative (First ("C1", "C2", "C3") ::CheckerSum )   
    it "Monad" $do  
      quickBatch $ monad (First ("C1", "C2", "C3") :: CheckerSum )   
    

