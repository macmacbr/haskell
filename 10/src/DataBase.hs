module DataBase where

import Data.Time

data DatabaseItem = DbString String 
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
    (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 8001
  , DbString "Hello, world!"
  , DbDate (UTCTime
    (fromGregorian 1921 5 1)
    (secondsToDiffTime 34123))
  ]

-- 1. Write a function that filters for DbDate values and returns a list
  -- of the UTCTime values inside them:
filterDbDate :: [DatabaseItem]
  -> [UTCTime]
filterDbDate = foldr (
  \a b -> case a of 
    DbDate u -> (:) u b
    _ -> b
  ) []
  
-- 2. Write a function that filters for DbNumber values and returns a list
  -- of the Integer values inside them:
filterDbNumber :: [DatabaseItem]
  -> [Integer]
filterDbNumber = foldr (
                   \a b -> case a of 
                     DbNumber u -> (:) u b
                     _ -> b
                   ) []

--3. Write a function that gets the most recent date:
mostRecent :: [DatabaseItem]
  -> UTCTime
mostRecent = mr . filterDbDate
  where
    mr [] = undefined
    mr (x:xs) = foldr (
                    \a b -> if a > b then a  else b 
                  ) x xs
-- 4. Write a function that sums all of the DbNumber values:

sumDb :: [DatabaseItem]
  -> Integer
sumDb = mr . filterDbNumber
          where
            mr [] = undefined
            mr (x:xs) = foldr (+) x xs
-- 5. Write a function that gets the average of the DbNumber values:
  -- You'll probably need to use fromIntegral
  -- to get from Integer to Double.
avgDb1 :: [DatabaseItem]
  -> Double  
avgDb1 db = (mr . sumDb) db
  where
    mr s = fromIntegral(s) / fromIntegral(length(filterDbNumber db))

-- AVG, single pass
avgDb :: [DatabaseItem]
  -> Double
avgDb = mr . filterDbNumber
          where
            upAvg x (av,n) = ((av * n + fromIntegral(x)) / (n+1), n+1)
            mr [] = undefined
            mr xs = fst $ foldr upAvg (0.0, 0.0) xs