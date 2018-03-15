module Exercises where
  
import Data.Monoid
import Test.Hspec  
import Test.QuickCheck 
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes  

lPure :: a -> [a]
lPure = pure

lAp :: [a -> b] -> [a] -> [b]
lAp = (<*>)

tExec = lAp f [1,2,3] 
  where f = lPure (*10)
    
iPure :: a -> IO a
iPure = pure

iAp :: IO (a -> b) -> IO a -> IO b
iAp = (<*>)

cartesianPure :: Monoid a => b -> (a,b)
cartesianPure = pure

cartesianAp :: Monoid a => (a, b -> c) -> (a, b) -> (a, c)
cartesianAp =  (<*>)

arrowPure :: b -> (a -> b)
arrowPure = pure

arrowAp :: (a -> ( b -> c)) -> (a -> b) -> (a -> c)
arrowAp = (<*>)

-- Pair

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a') 
  
instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') =  Pair (f a) (f' a')

-- Two
  
data Two a b = Two a b deriving (Eq, Show)  

instance Functor (Two a) where 
  fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
  pure b = Two (mempty) b
  (<*>) (Two a fb) (Two a' b) = Two (mappend a a') (fb b)  

--pure call Product mempty(*1)
twoEx =  pure ( * (Sum 10)) <*> Two (Product 10) (Sum 1)

-- Three 

data Three a b c = Three a b c deriving (Eq, Show)       
       
instance Functor (Three a b) where 
  fmap f (Three a b c) = Three a b (f c)       

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three (mempty) (mempty) c
  (<*>) (Three a b fc) (Three a' b' c) = Three (mappend a a') (mappend b b') (fc c)
       
       
-- Pair 
genPair :: ( Arbitrary a) => Gen (Pair a) 
genPair = do
  a' <- arbitrary
  a <- arbitrary
  return (Pair a a')
    
instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = genPair

instance (Eq a) => EqProp (Pair a) 
  where (=-=) = eq  

-- Two

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do 
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- Three

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = genThree

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq 

tests :: IO()
tests = hspec $ do
  describe "Laws" $ do
    it "Pair applicative" $ do
      quickBatch (applicative (Pair ("C1", "C2", "C3") ("C4", "C5", "C6") :: Pair (String, String, String))   )
    it "Two applicative" $ do
      quickBatch (applicative (Two (Sum 1, Sum 2, Sum 3) (Product 1, Product 2, Product 3)  :: Two (Sum Int, Sum Int, Sum Int) (Product Int, Product Int, Product Int) )   )
    it "Three applicative" $ do
      quickBatch (applicative (Three (Sum 1, Sum 2, Sum 3) (Product 1, Product 2, Product 3) ("A", "B", "C") :: Three (Sum Int, Sum Int, Sum Int) (Product Int, Product Int, Product Int) (String, String, String) )   )
    
    
    
      
          