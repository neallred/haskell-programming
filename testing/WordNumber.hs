module WordNumber where

import Data.List (intersperse)

-- numbers into words
digitToWord :: Int -> String
digitToWord x
  | x == 0 = "zero"
  | x == 1 = "one"
  | x == 2 = "two"
  | x == 3 = "three"
  | x == 4 = "four"
  | x == 5 = "five"
  | x == 6 = "six"
  | x == 7 = "seven"
  | x == 8 = "eight"
  | x == 9 = "nine"

digits :: Int -> [Int]
digits x = go x []
  where
    go num acc =
      let (tens, remainder) = divMod num 10
          result = remainder : acc
       in if tens == 0
            then result
            else go tens result

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
