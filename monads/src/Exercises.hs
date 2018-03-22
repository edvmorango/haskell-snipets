module Laws where
  
import Test.Hspec  
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes
import Test.QuickCheck
import Data.Monoid

-- Nope

data Nope a = NopeDotJPG deriving (Eq, Show)

instance Monoid a => Monoid (Nope a) where
  mempty = NopeDotJPG
  mappend _ _ = NopeDotJPG
  
instance Functor Nope where
  fmap _ _ = NopeDotJPG

instance Applicative Nope where
  pure _ = NopeDotJPG
  (<*>) _ _ = NopeDotJPG

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJPG  

instance (Arbitrary a) => Arbitrary (Nope a) where
  arbitrary = return NopeDotJPG

instance (Eq a) => EqProp (Nope a) where
  (=-=) = eq

--  Option

data CEither a b = CLeft a | CRight b deriving (Eq, Show)

instance Monoid b => Monoid (CEither a b) where
  mempty = CRight (mempty)
  mappend _ (CLeft a)  = CLeft a
  mappend (CLeft a) _ = CLeft a
  mappend (CRight b) (CRight b') = CRight (mappend b b')
  
instance Functor (CEither a) where
  fmap f (CRight b) = CRight (f b)
  fmap _ (CLeft a) = CLeft a
   
instance Applicative (CEither a) where
  pure b = CRight b
  (<*>) (CRight f) (CRight b) = CRight (f b)
  (<*>) _ (CLeft a) = CLeft a
  (<*>) (CLeft a) _  = CLeft a

instance Monad (CEither a) where
  return = pure
  (>>=) (CLeft a) _ = CLeft a  
  (>>=) (CRight b) f = f b


instance (Arbitrary a, Arbitrary b) => Arbitrary (CEither a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [CLeft a, CRight b]
    
    
instance (Eq a, Eq b) => EqProp (CEither a b) where
  (=-=) = eq

--

newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity  where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = pure (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

-- List

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil r = r
  mappend (Cons a n) r = Cons a (n <> r)

instance Functor (List) where
  fmap _ Nil = Nil
  fmap f (Cons a n) = Cons (f a) (fmap f n)

instance Applicative (List) where
  pure a = Cons a Nil 
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f n) l  = (fmap f l) <> (n <*> l) 

instance Monad (List) where
  return = pure 
  (>>=) Nil _ = Nil
  (>>=) (Cons a n) f =  (f a) <> ( n >>= f) 


instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do 
    a <- arbitrary 
    a' <- arbitrary
    a'' <- arbitrary
    elements [ Cons a (Cons a' Nil),  Cons a (Cons a' (Cons a'' Nil )), Nil]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

-- Monads 

j :: Monad m => m (m a) -> m a
j m =  m >>= id    
-- j [[1],[2],[3]] >>= 
-- id [1] <> id [2] <> id [3]
-- [1,2,3]

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m =  m >>= (return . f)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f m m' =  m >>= (\a -> m' >>=  (return . (f a))) 

l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2' f m m' = do
  a <- m
  b <- m'
  return (f a b)

l2Ap :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2Ap f m m' =  (return f) <*> m <*> m'

a :: Monad m => m a -> m (a -> b) -> m b
a m mf =  m >>= (\a -> mf >>= (\f ->  return (f a)))

a' :: Monad m => m a -> m (a -> b) -> m b
a' m mf =  do
  a <- m
  f <- mf
  return $ f a

aAp :: Monad m => m a -> m (a -> b) -> m b
aAp m mf =  mf <*> m

aApInterchange :: Monad m => m a -> m (a -> b) -> m b
aApInterchange m mf = (m >>= (\a -> pure ($ a))) <*> mf

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []  
meh (h:t) f = do
  b <- f h
  bs <- meh t f
  return $ b:bs  
    
-- meh :: Monad m => [a] -> (a -> mb) -> m [b]
-- meh la f = sequence $ la >>= (\a -> return $ f a) 

flipType :: (Monad m) => [m a] -> m [a]
flipType l = meh l id
-- [Just 1, Just 2, Just 3] 
--  id (Just 1), id (Just 2) : id (Just 3)
-- return (1 : 2 : 3)
-- Just [1,2,3]

type TestType = (String, Sum Int, Product Int)
type TestType2 = (String, String, String)

tests :: IO()
tests = hspec $ do
  describe "Laws" $ do
    describe "Nope" $ do
      it "Monoid" $ do
        quickBatch $ monoid (NopeDotJPG  :: Nope TestType  )
      it "Functor" $ do
        quickBatch $ functor (NopeDotJPG :: Nope  TestType)   
      it "Applicative" $ do
        quickBatch $ applicative (NopeDotJPG :: Nope TestType )   
      it "Monad" $do  
        quickBatch $ monad (NopeDotJPG ::  Nope TestType )       
    describe "CEither" $ do
      it "Monoid" $ do
        quickBatch $ monoid (CRight ("Hey", Sum 10, Product 2) :: CEither TestType2 TestType)
      it "Functor" $ do
        quickBatch $ functor (CRight ("Hey", Sum 10, Product 2) :: CEither TestType2 TestType)   
      it "Applicative" $ do
        quickBatch $ applicative (CRight ("Hey", Sum 10, Product 2) :: CEither TestType2 TestType)   
      it "Monad" $do  
        quickBatch $ monad (CRight ("Hey", Sum 10, Product 2) :: CEither TestType2 TestType)   
    describe "Identity" $ do
      it "Functor" $ do
        quickBatch $ functor (Identity ("Hey", Sum 10, Product 2) :: Identity TestType)   
      it "Applicative" $ do
        quickBatch $ applicative (Identity ("Hey", Sum 10, Product 2) :: Identity TestType)   
      it "Monad" $do  
        quickBatch $ monad (Identity ("Hey", Sum 10, Product 2) :: Identity TestType)   
    describe "List" $ do
      it "Monoid" $ do
        quickBatch $ monoid (Cons ("Hey", Sum 10, Product 2) Nil :: List TestType)
      it "Functor" $ do
        quickBatch $ functor (Cons ("Hey", Sum 10, Product 2) Nil :: List TestType)   
      it "Applicative" $ do
        quickBatch $ applicative (Cons ("Hey", Sum 10, Product 2) Nil :: List TestType)   
      it "Monad" $do  
        quickBatch $ monad (Cons ("Hey", Sum 10, Product 2) Nil :: List TestType)   
  
        
        
