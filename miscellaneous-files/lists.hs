module ListChapter where

import Data.Char

myHead (x : _) = x
trumm =  myHead [1, 2, 3]

myTail (_ : xs) = xs

safeTail :: [a] -> Maybe [a]
safeTail []       = Nothing
safeTail (x:[])   = Nothing
safeTail (_ : xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead []  = Nothing
safeHead (x:xs) = Just x

eftBool :: Bool -> Bool -> [Bool]
eftBool a b
  = enumFromTo a b

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b
  = enumFromTo a b

eftInt :: Int -> Int -> [Int]
eftInt a b
  = enumFromTo a b

eftChar :: Char -> Char -> [Char]
eftChar a b
  = enumFromTo a b

mySplit :: String -> Char -> [String]
mySplit stringToSplit splitChar = go stringToSplit splitChar []
  where go str char arr
          | (length str > 0) =
              go 
                ((dropWhile (== char)) . (dropWhile (/= char)) $ str)
                char
                (arr ++ [takeWhile (/= char) str])
          | otherwise = arr -- "wallfish" : (drop 1 arr)

-- dividedBy :: Integral a => a -> a -> (a, a)
-- dividedBy dividend divisor = go dividend divisor 0
--   where go n d count
--           | n < d = (count, n)
--           | otherwise =
--               go (n - d) d (count + 1)


-- go :: String -> [String] -> [String]
-- go stringSegment arr 
--   | (length stringSegment > 0) =
--       go 
--         ((dropWhile (== ' ')) . (dropWhile (/= ' ')) $ stringSegment)
--         (arr ++ [takeWhile (/= ' ') stringSegment])
--   | otherwise = "wallfish" : (drop 1 arr)

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
           \ symmetry?"


sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen 

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (mySplit sentences '\n' == shouldEqual)


zip' :: [a] -> [b] -> [(a,b)]
--zip' _ [] = []
--zip' [] _ = []
--zip' (x:xs) (y:ys) = ((x,y) : (zip' xs ys))
--
zip' a b = zipWith' (\a b -> (a,b)) a b

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = ((f x y) : (zipWith' f xs ys))

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = (toUpper x) : capitalize xs
