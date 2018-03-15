module Exercises where
  
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

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a') 
  
instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') =  Pair (f a) (f' a')
  
  
genPair :: ( Arbitrary a) => Gen (Pair a) 
genPair = do
  a' <- arbitrary
  a <- arbitrary
  return (Pair a a')
    
instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = genPair

-- Checkers


instance (Eq a) => EqProp (Pair a) 
  where (=-=) = eq  

tests :: IO()
tests = hspec $ do
  describe "Laws" $ do
    it "Pair applicative" $ do
      quickBatch (applicative (Pair ("C1", "C2", "C3") ("C4", "C5", "C6") :: Pair (String, String, String))   )
        