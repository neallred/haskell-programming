module Chapter9 where

import Data.Bool
import Data.Char

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

-- Exercises: Square Cube
squareCubeMySqr = [x ^ 2 | x <- [1 .. 5]]

squareCubeMyCube = [x ^ 3 | x <- [1 .. 5]]

-- 1a. zip squareCubeMySqr squareCubeMyCube
-- 1b. [(x ^ 2, x ^ 3) | x <- [1 .. 5]]
-- 2a. filter (\(x,y) -> x < 50 && y < 50) (zip squareCubeMySqr squareCubeMyCube)
-- 2b. [(x ^ 2, x ^ 3) | x <- [1 .. 5], x ^ 3 < 50]
-- 3. length [(x ^ 2, x ^ 3) | x <- [1 .. 5], x ^ 3 < 50]
-- Exercises: Bottom Madness
-- bottomMadness1 = [x ^ y | x <- [1, 2, 3, 4, 5], y <- [2, undefined]] -- blow up
-- 
-- bottomMadness2 = take 1 $ [x ^ y | x <- [1, 2, 3, 4, 5], y <- [2, undefined]] -- return 1
-- 
-- bottomMadness3 = sum [1, undefined, 3] -- blow up, has to evaluate all values
-- 
-- bottomMadness4 = length [1, 2, undefined] -- 3, it only recurses the spine
-- 
-- bottomMadness5 = length $ [1, 2, 3] ++ undefined -- blow up, it honly recurses the spine, however part of the spine itself is undefined
-- 
-- bottomMadness6 = take 1 $ filter even [1, 2, 3, undefined] -- [2] it lazily creates a constructor to create a list, but then it only takes the first element from the list constructor, so its ok.
-- 
-- bottomMadness7 take 1 $ filter even [1, 3, undefined] -- blow up, tries to read the values of the entire list because there are no even values in it, so it reaches undefined and blows up
-- 
-- bottomMadness8 take 1 $ filter odd [1, 3, undefined] -- [1], for the same reason as bottomMadness6
-- 
-- bottomMadness9 take 2 $ filter odd [1, 3, undefined] -- [1, 3], for the same reason as bottomMadness6
-- 
-- bottomMadness10 take 3 $ filter odd [1, 3, undefined] -- blow up, the first tow values are ok but it blows up on reaching the third value
-- Intermission: Is it in normal form?
-- 1. [1,2,3,4,5] NF
-- 2. 1 : 2 : 3 : 4 : _ WHNF (or neither?)
-- 3. enumFromTo 1 10 WHNF (or neither?)
-- 4. length [1, 2, 3, 4 5] WHNF
-- 5. sum (enumFromTo 1 10) neither
-- 6. neither
-- 7. NF
-- What does map exist for if fmap can do everything it does?
-- Exercises: More Bottoms
--------------------------
-- 1. take 1 $ map (+1) [undefined, 2, 3] - bottom.
-- 2. take 1 $ map (+1) [1, undefined, 3] - 2
-- 3. take 2 $ map (+1) [1, undefined, 3] - bottom
-- 4. Turns each character in a string into a Bool denoting whether the character was a vowel.
itIsMystery :: String -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs

moreBottoms5a = map (^ 2) [1 .. 10] == [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

moreBottoms5b = map minimum [[1 .. 10], [10 .. 20], [20 .. 30]] == [1, 10, 20]

moreBottoms5c = map sum [[1 .. 5], [1 .. 5], [1 .. 5]] == [15, 15, 15]

moreBottoms6 =
  map
    (\x -> bool x (-x) (x == 3) --if x == 3
         --then (-x)
         --else (x)) -- [1..10][1,2,-3,4,5,6,7,8,9,10]
     )

-- Exercises: Filtering
-----------------------
filtering1 x = mod x 3 == 0

filtering1Value = filter filtering1 [1 .. 30] == enumFromThenTo 3 6 30

filtering2 = length . filter filtering1

myFilter = filter (not . flip elem ["a", "an", "the"]) . words

-- Zipping exercises
--------------------
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : (zip' xs ys)

-- Chapter Exercises
--------------------
-- Data.Char
------------
-- 1.
isUpper' :: Char -> Bool
isUpper' = undefined

toUpper' :: Char -> Char
toUpper' = undefined

-- 2. To write a function that filters all the uppercase letters out of a String, use isUpper
uppersOnly = filter isUpper

-- 3.
capitalize (x:xs) = toUpper x : xs
capitalize "" = ""

-- 4.
upperAll (x:xs) = toUpper x : (capitalize xs)
upperAll "" = ""

-- 5.
unsafeUpperFirst x = toUpper (head x)

-- 6.
unsafeUpperFirst' x = toUpper . head $ x

unsafeUpperFirst'' = toUpper . head

-- Ciphers
----------
-- See chapter-09-cipher.ha (module Chapter9Cipher)
-- Writing your own standard functions
--------------------------------------
and' :: [Bool] -> Bool
and' [] = True
and' (False:_) = False
and' (_:xs) = and' xs

-- 1.
or' :: [Bool] -> Bool
or' [] = False
or' (True:_) = True
or' (_:xs) = or' xs

-- 2.
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = True
any' f xs = or' $ map f xs

-- 3.
elem' :: Eq a => a -> [a] -> Bool
elem' candidate [] = False
elem' candidate (x:xs)
  | candidate == x = True
  | otherwise = elem' candidate xs

-- 4.
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = x : reverse' xs

-- 5.
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- 6.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishMapWorks =
  and'
    [ squishMap (\x -> [1, x, 3]) [2] == [1, 2, 3]
    , squishMap (\x -> "WO " ++ [x] ++ " HOO ") "123" ==
      "WO 1 HOO WO 2 HOO WO 3 HOO "
    ]

-- 7.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8.
maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' f [x] = x
maximumBy' f (x1:x2:[]) =
  if f x1 x2 == GT
    then x1
    else x2
maximumBy' f (x1:x2:xs) =
  maximumBy'
    f
    ((if f x1 x2 == GT
        then x1
        else x2) :
     xs)

-- 9.
minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' f [x] = x
minimumBy' f (x1:x2:[]) =
  if f x1 x2 == LT
    then x1
    else x2
minimumBy' f (x1:x2:xs) =
  minimumBy'
    f
    ((if f x1 x2 == LT
        then x1
        else x2) :
     xs)

-- 10
maximum' :: (Ord a) => [a] -> a
maximum' = maximumBy' compare

minimum' :: (Ord a) => [a] -> a
minimum' = minimumBy' compare
