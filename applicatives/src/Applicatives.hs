module Applicatives where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes
import Test.Hspec

-- Generators

genT :: (Arbitrary a, Eq a) => Gen a
genT = do
  a <- arbitrary
  return a

genList :: (Arbitrary a, Eq a) => Gen a -> Gen [a]
genList a = do 
  as <- listOf a 
  return as


type FCompose = [Int] -> Fun Int Int -> Fun Int Int -> Bool

funCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
funCompose fu (Fun _ f)  (Fun _ g) = (fmap (g . f) fu) == ( (fmap g . fmap f) fu) 

type ACompose = Maybe Int ->  (Fun  Int Int) -> (Fun Int Int) -> Bool

apCompose :: (Eq (f c), Applicative f) => f a ->  (Fun a b ) -> (Fun b c) -> Bool
apCompose v  (Fun _ f)  (Fun _ g) = ((pure (.) <*> (pure g) <*> (pure f) <*> v )) == ((pure g) <*> ( (pure f) <*> v))

---Checkers

data Bull = Fo | Tw deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fo), (1, return Tw)]

instance Monoid Bull where
  mempty = Fo
  mappend Tw _ = Tw
  mappend _ Tw = Tw
  mappend _ _ = Fo
  
instance EqProp Bull where (=-=) = eq  


type TApplicative = (Char, Char, Char)



tests :: IO()
tests = hspec $ do
  describe "Functors - Without Checkers" $ do
    it "(Identity) -  id, fmapId and pureId are the same" $ do 
      forAll  (genList (genT :: Gen [Int])) 
        (\a -> let sid = id a  
                   fid = id <$> a
                   pid = (pure id) <*> a 
               in sid == fid && fid == pid )
    it "(Composition) -> pure (g ∘ f) <*> (F value) = (pure g) (pure f  <*> (F value))   " $ do
      quickCheck (apCompose :: ACompose )
    it "(Homomorphism) -> (pure function) <*>  (pure x) =  pure (function x) " $ do
      forAll (genT ::  Gen Int) 
        (\v ->  
          let ap =  pure (+1) <*> pure v 
              cp =  pure ((+1) v) :: Maybe Int
              in  ap == cp)
    it "(Interchange) -> (pure function) <*> (pure value)  ==  (pure  ($ value)) <*> (pure function) " $ do
      forAll (genT :: Gen Int) 
        (\v ->  
          let ap = pure (+1) <*> pure v 
              ic = pure ($ v) <*> pure (+1) :: Maybe Int
          in ap == ic
          )
    it "Monoid test with checkers" $ do 
         quickBatch (monoid Tw)  
    it "Applicative " $ do
          quickBatch (applicative [('a', 20 :: Int, 30 :: Double)] )
      
      
      
      
      
      
      
      