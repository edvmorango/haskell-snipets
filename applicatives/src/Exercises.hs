module Exercises where
  



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