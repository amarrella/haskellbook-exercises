import Data.Time
import Data.List

data DatabaseItem = DbString String 
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, word"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

getTime :: DatabaseItem -> [UTCTime]
getTime x = 
  case x of 
    DbDate t -> [t]
    _ -> []

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = 
  foldr (\a b -> (getTime a) ++ b) [] db

getNumber :: DatabaseItem -> [Integer]
getNumber x = 
  case x of 
    DbNumber n -> [n]
    _ -> []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = 
  foldr (\a b -> (getNumber a) ++ b) [] db

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = head . reverse . sort . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = 
  fromIntegral(sum nums) / fromIntegral(length nums)
    where nums = filterDbNumber db