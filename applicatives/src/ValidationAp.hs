module ValidationAp where

import Test.Hspec  
import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes  
  
data Validation e a = Failure e | Success a deriving (Eq, Show)


instance Functor (Validation e) where 
  fmap _ (Failure e) = Failure e 
  fmap f (Success e) = Success (f e)  
    
     
instance Monoid e => Applicative (Validation e) where
  pure a = Success a
  (<*>) (Success f) (Success a) = pure (f a)
  (<*>) (Success _) (Failure a) = Failure a
  (<*>) (Failure f) (Success _) = Failure f
  (<*>) (Failure f) (Failure a) = Failure (mappend f a)
  
  
  
genValidation :: ( Arbitrary e, Arbitrary a) => Gen (Validation e a) 
genValidation = do
  e <- arbitrary
  a <- arbitrary
  elements [Failure e, Success a]
  
instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary = genValidation


-- Checkers


instance (Eq e, Eq a) => EqProp (Validation e a) 
  where (=-=) = eq  
  
type V = Validation [Int] [Char]

tests :: IO()
tests = hspec $ do
  describe "Validation" $ do
    it "laws for String (String, String, String)" $ do
      quickBatch (applicative (Success ("C1", "C2", "C3") :: Validation String (String, String, String)  )   )
      
  
  
  
  
  
  