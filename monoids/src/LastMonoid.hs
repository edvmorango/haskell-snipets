module LastMonoid where

import Data.Monoid
import Test.QuickCheck

newtype Mem s a = Mem { runMem :: s -> (a, s) }


-- Assumption
-- CoArbitrary s = Defines Kind (Is necessary to compiler evaluate the function)
-- Arbitrary s = Defines Value
-- Arbitrary a = Defines Value
instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = do
    s <- arbitrary
    return $ Mem s


instance (Monoid a) => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty , s))
  mappend (Mem m) (Mem m') =  Mem $ \s ->
    let (a, s') = m s
        (a', s'') = m' s'
    in (a <> a' , s'')




f' = Mem $ \s -> ("hi", s+1)

---- Evalutation
-- Mem * -> * -> *
--  Mem $ \s -> ("Hi", s + 1)
--  Mem $ \s -> ( "Hi", s + 1) 0
--  Mem $ \0 -> ("Hi", 0 + 1)
--  Mem { runMem :: ("Hi", 1)}
--
---- Monoid Evalutation mempty
-- mempty = Mem \(s -> (mempty, s))
-- runMen $ mempty 10 ::  (String, Int)
-- Mem \s -> (mempty, s)
-- Mem \10 -> ((mempty :: String),10)
--
---- Monoid Append Evalutation
-- runMem (f' <> f') 5
-- mappend (Mem  \s -> ("hi", s + 1)) (Mem  \s -> ("hi", s + 1)) =
--    Mem $ \s -> (a, s' ) = m s
--    Mem $ \s -> (a, s'' ) = m' s'
--    Mem $ \s -> ("hi", (s + 1) ) = "hi" (s + 1)
--    Mem $ \s -> ("hi", ((s + 1) + 1 )) = m' (s + 1)
--  Apply  5
--    Mem $ \s -> ("hi", (5 + 1) ) = "hi" (5 + 1)
--    Mem $ \s -> ("hi", ((5 + 1) + 1) ) = m' (5 + 1)
--  In
--    ("hi" <> "hi", s''  )
--    ("hihi", 7)






lastm :: IO ()
lastm = do
  let rmzero = runMem mempty 10
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
