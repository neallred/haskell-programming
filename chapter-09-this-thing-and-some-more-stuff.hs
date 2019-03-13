module Chapter9 where

-- Exercise: enumFromTo
eft :: (Enum a) => a -> a -> [a]
eft start end = go start end []
  where
    go x y acc
      | fromEnum (succ x) > (fromEnum y) = []
      | otherwise = x : go (succ x) y acc

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

-- Exercises: Thy Fearful Symmetry
myWords :: String -> [String]
myWords str = go str []
  where
    go x acc
      | x == "" = acc
      | otherwise =
        takeWhile (/= ' ') x :
        (go (dropWhile (== ' ') (dropWhile (/= ' ') x)) acc)
