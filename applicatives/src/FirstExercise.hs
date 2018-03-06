module FirstExercise where
  
  

-- Partial function (Just (+3)) =  f (a -> b ) 
--  f ((+) 3 b)
added :: Maybe Integer 
added = Just (+3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])


-- Unecessary
y :: Maybe Integer
y =   (Just id) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])


z :: Maybe Integer
z =  (lookup 2 $ zip [1, 2, 3] [4, 5, 6])

-- chaning function application
tupled :: Maybe (Integer, Integer) 
tupled = Just (,) <*> y <*> z

