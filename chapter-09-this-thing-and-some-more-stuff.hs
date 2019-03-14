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
-- 1.
myWords :: String -> [String]
myWords str = go str []
  where
    go x acc
      | x == "" = acc
      | otherwise =
        takeWhile (/= ' ') x :
        (go (dropWhile (== ' ') (dropWhile (/= ' ') x)) acc)

-- 2.
firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen =
  "Could frame they fearful\
            \ symmetry"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual = lines sentences

splitString :: Char -> String -> [String]
splitString splitChar string = go splitChar string []
  where
    go x "" acc = acc
    go x y acc =
      go x (drop 1 . (dropWhile (/= x)) $ y) (acc ++ [takeWhile (/= x) y])

myLines :: String -> [String]
myLines = splitString '\n'

mainTigerTiger :: IO ()
mainTigerTiger =
  print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

-- 3.
myWords' = splitString ' '

-- Exercises: Comprehend Thy Lists
mySqr = [x ^ 2 | x <- [1 .. 10]]

-- [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
comprehendA = [x | x <- mySqr, rem x 2 == 0]

comprehendB = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]

comprehendC = take 5 [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]

guessA = [4, 16, 36, 64, 100]

guessB =
  [ (1, 64)
  , (1, 81)
  , (1, 100)
  , (4, 64)
  , (4, 81)
  , (4, 100)
  , (9, 64)
  , (9, 81)
  , (9, 100)
  , (16, 64)
  , (16, 81)
  , (16, 100)
  , (25, 64)
  , (25, 81)
  , (25, 100)
  , (36, 64)
  , (36, 81)
  , (36, 100)
  , (49, 64)
  , (49, 81)
  , (49, 100)
  ]

guessC = [(1, 64), (1, 81), (1, 100), (4, 64), (4, 81)]

comprehended =
  (comprehendA == guessA && comprehendB == guessB && comprehendC == guessC)
