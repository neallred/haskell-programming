module HangmanTest where

import Data.Maybe (isNothing)
import Main
import Test.Hspec
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
       if isNothing curr
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

propifiedFillPuzzle :: Puzzle -> Char -> Property
propifiedFillPuzzle init@(Puzzle (x:xs) _ _) char =
  True ==> propertyFillsInLetters init x

propFillPuzzle :: IO ()
propFillPuzzle = do
  (empty@(Puzzle str _ _):xs) <- sample' emptyPuzzleGen
  putStrLn str

runAllFillInCharacters =
  quickCheck propifiedFillPuzzle >> quickCheck propertyPreservesPuzzle >>
  quickCheck propertyAddsGuessed

-- puzzleGen = undefined
-- puzzleGen = do
--  (a :: String) <- arbitrary 
--  return (Puzzle map "")a
puzzle = Puzzle "trumm" [Nothing, Nothing, Just 'u', Nothing, Nothing] "u"

handleGuessSuite :: IO ()
handleGuessSuite =
  hspec $
  describe "handleGuess" $ do
    it "returns the puzzle unchanged if user guesses something already guessed" $ do
      actual <- handleGuess puzzle 'u'
      actual `shouldBe` puzzle
    it "adds correct letter to list of letters filled in and guessed" $ do
      actual <- handleGuess puzzle 'm'
      let expectedPuzzle =
            Puzzle "trumm" [Nothing, Nothing, Just 'u', Just 'm', Just 'm'] "mu"
      actual `shouldBe` expectedPuzzle
    it
      "adds correct letter to list of letters guessed but not to list of filled in" $ do
      actual <- handleGuess puzzle 'a'
      let expectedPuzzle =
            Puzzle "trumm" [Nothing, Nothing, Just 'u', Nothing, Nothing] "au"
      actual `shouldBe` expectedPuzzle
