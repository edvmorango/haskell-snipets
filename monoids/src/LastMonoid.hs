module LastMonoid where

import Data.Monoid

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance (Monoid a) => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty , s))
  mappend (Mem m) (Mem m') =  Mem $ \s ->
    let (a, s') = m s
        (a', s'') = m' s'
    in (a <> a' , s'')

f' = Mem $ \s -> ("hi", s + 1)

lastm :: IO ()
lastm = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
