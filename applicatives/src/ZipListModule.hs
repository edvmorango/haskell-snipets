module ZipListModule where
  
import Control.Applicative 
import Data.Monoid
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes


instance (Show a, Monoid a) => Monoid (ZipList a) where
  mempty = pure mempty -- lift a (mempty t) instance to ZipList
  mappend = liftA2 mappend 

-- instance Arbitrary a => Arbitrary (ZipList a) where
--   arbitrary = ZipList <$> arbitrary  
-- 
-- instance Arbitrary a => Arbitrary (Sum a) where
--   arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq



data List a = Nil | Cons a (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ (Cons a Nil) = Cons a Nil
take' 1 (Cons a _) = Cons a Nil
take' n (Cons a (next)) = Cons a (take' (n-1) next)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil a = a 
  mappend (Cons a next) rl = Cons a (mappend next rl)
  
instance Functor (List) where
  fmap f (Nil)  = Nil 
  fmap f (Cons a (next) ) =  Cons (f a ) (fmap f next) 

instance Applicative (List) where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f n) l  = (fmap f l) <> (n <*> l) 
       
newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

-- `eq` comes from Checkers
instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l


newtype ListZ a = ListZ (List a)

instance  Monoid a => Monoid (ListZ a) where
  mempty = ListZ Nil
  mappend (ListZ a) (ListZ  b) = ListZ (zipper a b)
    where zipper Nil _ = Nil
          zipper _ Nil = Nil
          zipper (Cons w nw) (Cons w' nw') = (Cons (w <> w') (zipper nw nw') )    

instance Functor ZipList' where 
  fmap f (ZipList' xs) = 
    ZipList' $ fmap f xs

-- (f a -> b) -> f a -> f b
-- instance Applicative ZipList' where 
  -- pure a = ZipList' (pure a) 
  -- (<*>) (ZipList' Nil) _ = ZipList' Nil
  -- (<*>) _ (ZipList' Nil) = ZipList' Nil
  -- (<*>) (ZipList' (Cons c n)) (ZipList' l  ) = ZipList' ((fmap c l) <> (n <*>  (tail )))
 
-- tests :: IO()
-- tests = hspec $ do
--   describe "ZipList tests" $ do
--     it "take " $ do
--        True ==  True