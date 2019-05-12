module HangmanTest where

import Main
import Test.QuickCheck

arbitraryString :: Gen String
arbitraryString = arbitrary

alphaNums = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']

emptyPuzzleGen :: Gen Puzzle
emptyPuzzleGen = do
  char1 <- elements alphaNums
  char2 <- elements alphaNums
  char3 <- elements alphaNums
  char4 <- elements alphaNums
  let str = char1 : char2 : char3 : char4 : ""
  return (Puzzle str (map (const Nothing) str) "")

instance Arbitrary Puzzle where
  arbitrary = emptyPuzzleGen

propertyAddsGuessed :: Puzzle -> Char -> Bool
propertyAddsGuessed initial@(Puzzle puzzle progress guessed) char =
  let result@(Puzzle newPuzzle newProgress newGuessed) =
        fillInCharacter initial char
   in length newGuessed > length guessed

countJust :: (Eq a) => [Maybe a] -> Int
countJust =
  foldr
    (\curr acc ->
       acc +
       if curr == Nothing
         then 0
         else 1)
    0

propertyFillsInLetters :: Puzzle -> Char -> Bool
propertyFillsInLetters initial@(Puzzle puzzle progress guessed) char =
  let result@(Puzzle newPuzzle newProgress newGuessed) =
        fillInCharacter initial char
   in countJust newProgress > countJust progress

propertyPreservesPuzzle :: Puzzle -> Char -> Bool
propertyPreservesPuzzle initial@(Puzzle puzzle _ _) char =
  let Puzzle newPuzzle _ _ = fillInCharacter initial char
   in newPuzzle == puzzle

propertyFillPuzzle2 :: Puzzle -> Char -> Property
propertyFillPuzzle2 init@(Puzzle (x:xs) _ _) char =
  True ==> propertyFillsInLetters init x

propertyFillPuzzle = quickCheck propertyFillPuzzle2

propFillPuzzle :: IO ()
propFillPuzzle = do
  (empty@(Puzzle str _ _):xs) <- sample' emptyPuzzleGen
  putStrLn str
-- puzzleGen = undefined
-- puzzleGen = do
--  (a :: String) <- arbitrary 
--  return (Puzzle map "")a
--
--
-- 5.
-- propQuotRem :: Int -> Int -> Bool
-- propQuotRem x y = (quot x y) * y + (rem x y) == x
-- 
-- propertyQuotRem :: Int -> Int -> Property
-- propertyQuotRem x y = x /= 0 && y /= 0 ==> propQuotRem x y
