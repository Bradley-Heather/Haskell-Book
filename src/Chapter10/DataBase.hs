module DataBase where

import Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime 
     deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
   [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
   , DbNumber 9001
   , DbString "Hello, world!"
   , DbNumber 3001
   , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
   ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = foldr go [] db
  where go (DbDate x) y = x : y
        go _ y = y

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = foldr go [] db
   where go (DbNumber x) y = x : y
         go _ y = y 

mostRecent :: [DatabaseItem] -> UTCTime 
mostRecent = minimum . filterDbDate

sumDb :: [DatabaseItem] -> Integer 
sumDb = sum . filterDbNumber

avgDb ::  [DatabaseItem] -> Double
avgDb db = fromIntegral total / fromIntegral count
   where total = length $ filterDbNumber db
         count = sum $ filterDbNumber db