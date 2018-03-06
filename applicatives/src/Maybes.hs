module Maybes where
import Control.Applicative  
  
  
data Name = FirstName String deriving (Eq, Show)
data Age = Age Int deriving (Eq, Show)

data Person = Person Name Age deriving (Eq, Show)  


mkNameV :: String -> Maybe Name
mkNameV n = if (length n) > 10
  then Nothing 
  else Just (FirstName n)

mkAgeV :: Int -> Maybe Age
mkAgeV a = if (a >= 18)
  then Just (Age a)
  else Nothing


mkPersonV :: String -> Int -> Maybe Person
mkPersonV n a = case (mkNameV n) of
  Nothing -> Nothing
  Just name -> case (mkAgeV a) of
    Nothing -> Nothing
    Just age -> Just (Person name age)

mkPersonLV :: String -> Int -> Maybe Person
mkPersonLV n a = appliedName <*> mkAgeV a
  where appliedName = Person <$> (mkName n)
      
      

      
mkName :: String -> Maybe Name
mkName n = FirstName <$> (val n)
  where val = (\v -> if (length v) > 10 then Nothing else Just v)     

mkAge :: Int -> Maybe Age
mkAge a = Age <$> (val a)
  where val = (\v -> if v >= 18 then (Just v) else Nothing)

mkPerson :: String -> Int -> Maybe Person
mkPerson n a = Person <$> (mkName n) <*> (mkAge a)


-- Remember, ADTs and functions are almost the same thing in terms of categories
-- a ADT application, is a function 
mkPerson' :: String -> Int  -> Maybe Person
mkPerson' n a = liftA2 Person (mkName n) (mkAge a)







