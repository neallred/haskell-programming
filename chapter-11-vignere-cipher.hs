module Chapter11VignereCipher where

import Data.Char
import System.IO
import Text.Read (readMaybe)

-- ord
-- chr
minCap = ord 'A' :: Int

maxCap = ord 'Z' :: Int

minLower = ord 'a' :: Int

maxLower = ord 'z' :: Int

checkLower = flip elem [minLower .. maxLower]

checkCap = flip elem [minCap .. maxCap]

toCharacterPad :: Char -> Int
toCharacterPad x =
  let letterInt = ord x
   in if checkLower letterInt
        then letterInt - minLower
        else if checkCap letterInt
               then letterInt - minCap
               else 0

toStringPad :: String -> [Int]
toStringPad "" = repeat 0
toStringPad x = concat . repeat . map toCharacterPad $ x

shiftLetter :: Int -> (Char, Int) -> Char
shiftLetter 0 (letter, padShift) = letter
shiftLetter shiftAmount (letter, padShift) =
  let letterInt = ord letter
      isLower = checkLower letterInt
      isUpper = checkCap letterInt
      isUnshiftable = not isLower && not isUpper
   in if isUnshiftable
        then letter
        else let effectiveMin =
                   if isLower
                     then minLower
                     else minCap
                 effectiveMax =
                   if isLower
                     then maxLower
                     else maxCap
              in chr $
                 effectiveMin +
                 mod
                   (letterInt + shiftAmount + padShift - effectiveMin)
                   (effectiveMax - (effectiveMin - 1))

joinMessageAndPad :: String -> [Int] -> [(Char, Int)]
joinMessageAndPad "" _ = []
joinMessageAndPad _ [] = []
joinMessageAndPad (char:chars) pad@(padInt:padInts)
  | checkLower (ord char) || checkCap (ord char) =
    (char, padInt) : joinMessageAndPad chars padInts
  | otherwise = (char, 0) : joinMessageAndPad chars pad

makeCaesar :: Int -> [Int] -> String -> String
makeCaesar shiftBy pad toEncode = map (shiftLetter shiftBy) (zip toEncode pad)

biumvirate :: Int -> String -> (String -> String, String -> String)
biumvirate x padString =
  ( makeCaesar (negate (abs x)) (map negate $ toStringPad padString)
  , makeCaesar (abs x) (toStringPad padString))

(uncaesar13, caesar13) = biumvirate 13 ""

getIntFromUser :: IO Int
getIntFromUser = do
  userInput <- getLine
  case readMaybe userInput of
    Nothing -> do
      putStrLn "(integer input required, please try again)"
      getIntFromUser
    Just n -> return n

getYesNo :: IO Bool
getYesNo = do
  char <- getChar
  case char of
    'y' -> do
      putStrLn ""
      return True
    'n' -> do
      putStrLn ""
      return False
    _ -> do
      putStrLn "\nPlease enter \"y\" for encode, \"n\" for decode"
      getYesNo

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Enter a shift amount:"
  shiftAmount <- getIntFromUser
  putStrLn "Enter a pad:"
  pad <- getLine
  putStrLn "Should your input message be encoded (y) or decoded (n)?"
  shouldEncode <- getYesNo
  putStrLn "Enter message for ciphery:"
  messageForCiphery <- getLine
  putStrLn $
    let (decode, encode) = biumvirate shiftAmount pad
     in if shouldEncode
          then encode messageForCiphery
          else decode messageForCiphery
