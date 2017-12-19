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


foldBool :: a -> a -> Bool -> a
foldBool a b c = case c of
                 False -> a
                 True -> b

multiplesFilter :: (Integral a) => a -> a -> Bool
multiplesFilter b a =  a `mod` b == 0

filterMultiplesOf3 :: (Integral a) => [a] -> [a]
filterMultiplesOf3 = filter (multiplesFilter 3)

lengthFilter :: (Integral a) => [a] -> Int
lengthFilter a = (length . filterMultiplesOf3) a

filterString :: String -> Bool
filterString "a" = False
filterString "the" = False
filterString "an" = False
filterString _ = True

toStringList :: String -> [String]
toStringList = (filter filterString . withOperator ' ')


zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:at) (b:bt) =  (a,b) : zip' at bt

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:at) (b:bt) = f a b : zipWith' f at bt

zipInWith :: a -> b -> (a,b)
zipInWith a b = (a,b)

zipInTermsOfZipWith :: [a] -> [b] -> [(a,b)]
zipInTermsOfZipWith a b = zipWith' zipInWith a b
