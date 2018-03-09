module ZipListModule where
  
import Control.Applicative 
import Data.Monoid
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes


instance (Show a, Monoid a) => Monoid (ZipList a) where
  mempty = pure mempty -- lift mempty a instance to ZipList
  mappend = liftA2 mappend 

-- instance Arbitrary a => Arbitrary (ZipList a) where
--   arbitrary = ZipList <$> arbitrary  
-- 
-- instance Arbitrary a => Arbitrary (Sum a) where
--   arbitrary = Sum <$> arbitrary


instance Eq a => EqProp (ZipList a) where
  (=-=) = eq
