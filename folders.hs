module Folders where

import Data.Time
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
     (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 1
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate db =  foldr filtr [] db
  where filtr a b =  case a of
                    DbDate time -> time : b
                    _ -> b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber db = foldr filtr [] db
  where filtr a b =  case a of
                    DbNumber n -> n : b
                    _ ->  b

getMostRecent :: [DatabaseItem] -> UTCTime
getMostRecent = (maximum . filterDbDate)

sumDb :: [DatabaseItem] -> Integer
sumDb = (sum . filterDbNumber)


avgDb :: [DatabaseItem] -> Double
avgDb db =  (fromIntegral $ sum numbers) / (fromIntegral $ length numbers)
  where numbers = filterDbNumber db
