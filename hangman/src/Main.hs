module Main where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.List (intersperse, sort)
import Data.Maybe (fromMaybe, isJust)
import System.Exit (exitSuccess)
import System.IO
import System.Random (randomRIO)

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

isSensibleWord :: String -> Bool
isSensibleWord = all (`elem` (['A' .. 'Z'] ++ ['a' .. 'z']))

gameWords :: IO WordList
gameWords = do
  WordList aw <- allWords
  return $ WordList (filter isSensibleWord (filter gameLength aw))
  where
    gameLength w =
      let l = length (w :: String)
       in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle String
         [Maybe Char]
         String

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'

instance Show Puzzle where
  show puzzle@(Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered) ++
    " Wrong letters: " ++ wrongLetters puzzle

freshPuzzle :: String -> Puzzle
freshPuzzle xs = Puzzle xs (map (const Nothing) xs) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle puzzle _ _) x = x `elem` puzzle

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) x = x `elem` guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (sort (c : s))
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn
        "You already guessed that\
                 \ character, pick \
                 \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn
        "This character was in the\
                \ word, filling in the word\
                \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn
        "This character wasn't in\
                \ the word, try again."
      return (fillInCharacter puzzle guess)

boundedInt :: Int -> Int -> Int -> Int
boundedInt minimum maximum number
  | number < minimum = minimum
  | number > minimum = maximum
  | otherwise = number

zeroToZeven = boundedInt 0 7

baseWidth = 14 :: Int

halfWidth = baseWidth `div` 2

halfLessOne = replicate halfWidth

leftBase = halfLessOne '_' ++ "|"

rightBase = "|" ++ halfLessOne '_'

emptyPoleLeft = halfLessOne ' ' ++ "|"

emptyPole = emptyPoleLeft ++ "|" ++ halfLessOne ' ' ++ "\n"

baseFloor = leftBase ++ rightBase ++ "\n"

base = emptyPole ++ emptyPole ++ emptyPole ++ baseFloor

mast = emptyPoleLeft ++ "|---|\n"

bodyHead :: String
bodyHead = emptyPoleLeft ++ "|" ++ replicate (halfWidth - 4) ' ' ++ "0\n"

neck = emptyPoleLeft ++ "|" ++ "   |\n"

neckLeftArm = emptyPoleLeft ++ "|" ++ " --|\n"

neckLeftArmRightArm = emptyPoleLeft ++ "|" ++ " --|--\n"

trunk = emptyPoleLeft ++ "|" ++ "   |\n"

leftFoot = emptyPoleLeft ++ "|" ++ "  /\n"

leftFootRightFoot = emptyPoleLeft ++ "|" ++ "  / \\\n"

headerFooter x = mast ++ x ++ base

-- render :: Int -> String
-- render x
--   | x <= 0 = ""
drawBody :: Int -> String
drawBody x
  | x <= 0 = headerFooter (concat $ replicate 4 emptyPole)
  | x == 1 = headerFooter (bodyHead ++ concat (replicate 3 emptyPole))
  | x == 2 = headerFooter (bodyHead ++ neck ++ concat (replicate 2 emptyPole))
  | x == 3 =
    headerFooter (bodyHead ++ neckLeftArm ++ concat (replicate 2 emptyPole))
  | x == 4 =
    headerFooter
      (bodyHead ++ neckLeftArmRightArm ++ concat (replicate 2 emptyPole))
  | x == 5 =
    headerFooter (bodyHead ++ neckLeftArmRightArm ++ trunk ++ emptyPole)
  | x == 6 = headerFooter (bodyHead ++ neckLeftArmRightArm ++ trunk ++ leftFoot)
  | x >= 7 =
    headerFooter (bodyHead ++ neckLeftArmRightArm ++ trunk ++ leftFootRightFoot)

maxErrors = 7

wrongLetters :: Puzzle -> String
wrongLetters (Puzzle wordToGuess _ guessed) =
  filter (`notElem` wordToGuess) guessed

numberWrong :: Puzzle -> Int
numberWrong = length . wrongLetters

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle wordToGuess _ guessed) =
  when (numberWrong puzzle > maxErrors) $ do
    putStrLn "You lose!"
    putStrLn $ "The word was: " ++ wordToGuess
    exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle thingToGuess filledInSoFar _) =
  when (all isJust filledInSoFar) $ do
    putStrLn $ "You win! The word was " ++ thingToGuess
    exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle =
  forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStrLn (drawBody (numberWrong puzzle))
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle (toLower c) >>= runGame
      _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
