{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StateMain where

import Control.Applicative (liftA3)
import Control.Monad (join, replicateM)
import Control.Monad.Trans.State
import System.Random

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    _ -> error $ "Invalid n: " ++ (show n)

rollThreeTimes :: (Die, Die, Die)
rollThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie =
  state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollThreeTimes' :: State StdGen (Die, Die, Die)
rollThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

-- Moi( "state" in francese)
newtype Moi s a = Moi
  { runMoi :: s -> (a, s)
  }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi v) =
    Moi $ \s ->
      let (output, outputState) = (v s)
       in ((f output), outputState)

--             (a -> b)    f s a      a  s    
useFunctor = runMoi ((+ 1) <$> (Moi (\s -> (0, s)))) 0

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))
  (<*>) :: (Moi s (a -> b)) -> Moi s a -> Moi s b
  (<*>) (Moi f) (Moi b) =
    Moi $ \s ->
      let (fo, fos) = f s
          (bo, bos) = b fos
       in (fo bo, bos)

-- firstArg 0 
-- \0 -> (2 + 0, 0+1)
-- (2,1)
firstArg :: Moi Int Int
firstArg = Moi (\s -> (2 + s, s + 1))

-- firstArg 1 
-- \1 -> (1 + 1, 1 * 2)
-- (2,2)
secondArg :: Moi Int Int
secondArg = Moi (\s -> (1 + s, s * 2))

useApplicative = runMoi ((+) <$> firstArg <*> secondArg)

instance Monad (Moi s) where
  return :: a -> Moi s a
  return = pure
  (>>=) :: (Moi s a) -> (a -> Moi s b) -> (Moi s b)
  (>>=) (Moi a) f =
    Moi $ \s ->
      let (o, ns) = a s
          (Moi fb) = f o
       in fb ns

-- firstArg' 10 
-- \s -> (10, s +1)
firstArg' :: Int -> Moi Int Int
firstArg' a = Moi (\s -> (a, s + 1))

-- secondArg' 10
-- \s -> ("10",  s + 1)
secondArg' :: Int -> Moi Int String
secondArg' a = Moi (\s -> (show a, s + 1))

-- runMoi (useMonadic 10) 0
--  (10, 1)  <- firstArg' 10 
--  ("10", 2) <- secondArg'10
useMonadic :: Int -> Moi Int String
useMonadic a = do
  v <- firstArg' a
  v' <- secondArg' v
  return v'

-- useMonadic' 10 0
-- runMoi starts the functorial context
-- firstArg' and secondArg'
useMonadic' :: Int -> Int -> (String, Int)
useMonadic' a s =
  let (res :: Moi Int String) = do
        v <- firstArg' a
        v' <- secondArg' v
        return v'
   in runMoi res s

useMonadic'' :: Int -> Int -> (String, Int)
useMonadic'' a s = runMoi (firstArg' a >>= secondArg') s
-- useMonadic a s = runMoi (...) s     starts the context
-- useMonadic 10 0 = runMoi (...) 0 
-- (>>=) :: (Moi Int Int) -> (Int -> Moi Int Int ) -> Moi Int Int 
-- Moi 0 10  -> (10 -> Moi Int Int) -> Moi Int Int
-- (>>=) (Moi 0 10) (10 -> Moi Int Int) 
-- Moi $ \0 ->
--     let (10, 0)  = (0 -> (10, 0)) 0
--         (Moi 10) = f 10  (implicit 1 ?)
--     
--
--
