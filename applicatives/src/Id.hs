module Id where
  
  
newtype Identity a = Identity a deriving (Eq, Ord, Show) 



instance Functor Identity where 
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where 
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = pure (f a)


newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where 
  fmap _ (Constant a)  = Constant (a)

instance Monoid a => Applicative (Constant a) where
  pure a = Constant mempty
  (<*>) (Constant f) (Constant a ) = Constant (mappend f a) 