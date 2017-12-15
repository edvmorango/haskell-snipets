module BasicDataType where

data Mood = Blah | Woot deriving Show


awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

v = length (concat allAwesome)

isPalindrome :: String -> Bool
isPalindrome x =  reverse x == x


f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f fi s =  ((snd fi, snd s),(fst fi, fst s))
