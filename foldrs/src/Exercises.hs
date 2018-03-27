module Exercises where
  
import Test.Hspec  
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes
import Test.QuickCheck
import Data.Monoid
  

genT :: (Arbitrary a, Eq a, Ord a, Monoid a) => Gen a
genT = do
  a <- arbitrary
  return a
    
data Constant a b = Constant b deriving (Eq, Show)

newtype Constants a b = Constants [Constant a b]

instance (Monoid b) => Monoid (Constant a b) where
  mempty = Constant (mempty)
  mappend (Constant b) (Constant b') = Constant (b `mappend` b')

instance Foldable (Constant a) where
  foldr f acc (Constant c) = f c acc
  foldl f acc (Constant c) = f acc c
  foldMap f (Constant c) = f c



genConstant :: (Arbitrary a, Arbitrary b, Monoid b) => Gen (Constant a b) 
genConstant = do
    b <- arbitrary
    return $ Constant b

genConstants :: (Arbitrary a, Arbitrary b, Monoid b) => Gen b -> Gen ([(Constant a b)], [b])
genConstants g = do
    l <- listOf g
    return $ ( (fmap (Constant) l), l)


instance (Arbitrary a, Arbitrary b, Monoid b) => Arbitrary (Constant a b) where
  arbitrary = genConstant

  
  
instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq


data Two a b = Two a b  deriving (Eq, Show)

data Three a b c = Three b c deriving (Eq, Show)

data Three' a b = Three' a b b deriving (Eq, Show)

data Four a b = Four a b b b  deriving (Eq, Show)


type PhantomType = ( () , () ,())
type TestType = (String, Sum Int, Product Int)

tests :: IO()
tests = hspec $ do
  describe "Constant" $ do
      it "Constant a b" $ do
        quickBatch $ monoid ( Constant ("Hey", Sum 20, Product 10 ) :: Constant PhantomType TestType  )
      it "foldr == foldMap" $ do 
        forAll ( (genConstants (genT :: Gen (Product Int)) ) :: Gen ([Constant Int (Product Int)], [Product Int])) 
                (\(w, u) -> foldr (\c ac -> (mappend c ac)) (Constant (Product 1)) w == foldMap (Constant) u   ) 

