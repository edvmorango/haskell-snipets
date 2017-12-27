module Phone where

import Data.Char
import Data.List

type Digit = Char
type Presses = Int
type Values = String
data DaPhone = DaPhone [(Digit, Values)] deriving (Eq, Show)

keyboard = DaPhone
  [  ('1', "1")
   , ('2', "abc2" )
   , ('3', "def3")
   , ('4', "ghi4")
   , ('5', "jkl5")
   , ('6', "mno6")
   , ('7', "pqrs7")
   , ('8', "tuv8")
   , ('9', "wxyz9")
   , ('0', " +_0")
   , ('*', "*^")
   , ('#', ".,")
  ]


convo :: [String]
convo =
       ["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Just making sure rofl ur turn"]



calcPresses :: Digit -> (Digit, Values) -> [(Digit, Presses)]
calcPresses c (d,v)
  | isUpper c = ('*', 1) : calcPresses (toLower c) (d,v)
  | otherwise = case (elemIndex c v) of
                  Just v ->  [(d, v + 1)]
                  Nothing -> []

reverseTaps :: DaPhone -> Digit -> [(Digit, Presses)]
reverseTaps (DaPhone kb) c =
  case (find (\(_,b) -> elem (toLower c) b) kb) of
    Just a -> calcPresses c a
    Nothing -> []


cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead _ []  = []
cellPhonesDead k (h:t) = (reverseTaps k h)  ++ cellPhonesDead k t


fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps a = foldr (\(_,p) acc -> p + acc ) 0 a

--- Hard Way
removeFromList :: (Eq a) => a -> [a] -> [a]
removeFromList _ [] = []
removeFromList a (h:t) =
  if h == a
    then removeFromList a t
    else h : removeFromList a t

groupList :: String -> [(Char,Int)] -> [(Char,Int)]
groupList [] acc = acc
groupList (h:t) acc =
    case find (\ (b,_) -> lh == b) acc of
      Just (i,v) -> groupList t  ( (lh, v + ic) : removeFromList (i,v) acc)
      Nothing ->  groupList t ( (lh, ic) : acc)
    where lh = toLower h
          upper = isUpper h
          ic = if upper then 2 else 1

mostPopular :: String -> (Char,Int)
mostPopular = (maximumBy (\(_,a) (_,b) -> compare a b)  . (flip groupList []))

mostPopularLetter :: String -> Char
mostPopularLetter = ( fst . mostPopular)

coolestLtr :: [String] -> Char
coolestLtr =   ( fst . (maximumBy (\(_,a) (_,b) -> compare a b) . map (mostPopular)) )


groupListString :: [String] -> [(String,Int)] -> [(String,Int)]
groupListString [] acc = acc
groupListString (h:t) acc =
    case find (\ (b,_) ->  lh == b) acc of
      Just (i,v) -> groupListString t  ( (lh, v + 1) : removeFromList (i,v) acc)
      Nothing ->  groupListString t ( (lh, 1) : acc)
    where lh = map (toLower) h

coolestWord :: [String] -> String
coolestWord =  (fst . (maximumBy (\(_,a) (_,b) -> compare a b) . (flip groupListString [])))
