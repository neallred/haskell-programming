--numbers-into-words.hs

module WordNumber where

import Data.List (intersperse)

digitWords =
  [ "zero"
  , "one"
  , "two"
  , "three"
  , "four"
  , "five"
  , "six"
  , "seven"
  , "eight"
  , "nine"
  ]

digitToWord :: Int -> String
digitToWord n = digitWords !! mod n 10

digits :: Int -> [Int]
digits n = go (abs n) []
  where go n acc
          | fst (divMod n 10) == 0 = snd (divMod n 10):acc
          | otherwise = go (fst (divMod n 10)) (snd (divMod n 10):acc)

wordNumber :: Int -> String
wordNumber = concat . (intersperse "-") . (map digitToWord) . digits



