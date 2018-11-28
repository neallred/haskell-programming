import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)


theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbNumber 9000
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem]
             -> [UTCTime]
filterDbDate [] = []
filterDbDate ((DbDate date):xs) = [date] ++ (filterDbDate xs)
filterDbDate (_:xs) = (filterDbDate xs)

filterDbNumber :: [DatabaseItem]
               -> [Integer]
filterDbNumber [] = []
filterDbNumber ((DbNumber x):xs) = [x] ++ (filterDbNumber xs)
filterDbNumber (_:xs) = (filterDbNumber xs)

sumDb :: [DatabaseItem]
      -> Integer
sumDb x = foldl (+) 0 $ filterDbNumber x

avgDb :: [DatabaseItem]
      -> Double
avgDb x = (fromIntegral (foldl (+) 0 $ filterDbNumber x)) / (fromIntegral (length (filterDbNumber x)))


