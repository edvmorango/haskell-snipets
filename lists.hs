module Lists where

-- Honestly, I really did not understand this exercise

-- eftBool :: Bool -> Bool -> [Bool]
-- eftBool a b =  a : b : []

-- eftOrd :: Ordering -> Ordering -> [Ordering]
-- eftOrd = undefined

-- eftInt :: Int -> Int -> [Int]
-- eftInt = undefined

-- eftChar :: Char -> Char -> [Char]
-- eftChar = undefined

myWords :: String -> [String]
myWords [] = []
myWords a = takeWhile(/=' ') a : myWords (drop  1 $ dropWhile(/=' ') a)


myLines :: String -> [String]
myLines [] = []
myLines a = takeWhile(/='\n') a : myLines (drop  1 $ dropWhile(/='\n') a)

withOperator :: Char -> String -> [String]
withOperator _ [] = []
withOperator s a = takeWhile(/= s) a : withOperator s (drop  1 $ dropWhile(/=s) a)


mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
myTuple = [(x,y) |  x <- mySqr, y <- myCube, x < 50, y < 50 ]
myTupleL = length myTuple
