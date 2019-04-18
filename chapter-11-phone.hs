module Chapter11Phone where

import Data.Char
import Data.List
import qualified Data.Map as Map

-- 1. Create the data structure
data Key
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Zero
  | Star
  | Pound
  deriving (Eq, Show)

data KeyChars =
  KeyChars Key
           String

keyCharsOne = KeyChars One "1"

keyCharsTwo = KeyChars Two "abc2"

keyCharsThree = KeyChars Three "def3"

keyCharsFour = KeyChars Four "ghi4"

keyCharsFive = KeyChars Five "jkl5"

keyCharsSix = KeyChars Six "mno6"

keyCharsSeven = KeyChars Seven "pqrs7"

keyCharsEight = KeyChars Eight "tuv8"

keyCharsNine = KeyChars Nine "wxyz9"

keyCharsZero = KeyChars Zero "+ _0"

keyCharsPound = KeyChars Pound ".,#"

keyCharsStar = KeyChars Star ""

allKeyChars =
  [ keyCharsOne
  , keyCharsTwo
  , keyCharsThree
  , keyCharsFour
  , keyCharsFive
  , keyCharsSix
  , keyCharsSeven
  , keyCharsEight
  , keyCharsNine
  , keyCharsZero
  , keyCharsPound
  , keyCharsStar
  ]

alphabet = foldr (\(KeyChars _ chars) acc -> chars ++ acc) "" allKeyChars

type Presses = Int

type Keypress = (Key, Presses)

getMatchingChars :: Key -> String
getMatchingChars key =
  let (KeyChars _ chars) =
        case key of
          One -> keyCharsOne
          Two -> keyCharsTwo
          Three -> keyCharsThree
          Four -> keyCharsFour
          Five -> keyCharsFive
          Six -> keyCharsSix
          Seven -> keyCharsSeven
          Eight -> keyCharsEight
          Nine -> keyCharsNine
          Zero -> keyCharsZero
          Pound -> keyCharsPound
          Star -> keyCharsStar
   in chars

-- 2. Convert conversations into keypresses.
mFindIndex :: (Eq a) => [a] -> a -> Maybe Int
mFindIndex [] _ = Nothing
mFindIndex xs toFind = go 0 xs
  where
    go index [] = Nothing
    go index (x:xs)
      | x == toFind = Just index
      | otherwise = go (index + 1) xs

mIndex :: [a] -> Int -> Maybe a
mIndex [] _ = Nothing
mIndex (x:xs) index
  | index < 0 = Nothing
  | index == 0 = Just x
  | otherwise = mIndex xs (index - 1)

keypressToString :: Keypress -> String
keypressToString (Star, presses) = ""
keypressToString (key, presses) =
  let matchingChars = getMatchingChars key
      numChars = length matchingChars
      (_, position) = divMod presses numChars
      indexToAccess =
        (if position == 0
           then numChars
           else position) -
        1
   in case mIndex matchingChars indexToAccess of
        Just char -> char : ""
        Nothing -> ""

upperFirst :: String -> String
upperFirst "" = ""
upperFirst (x:xs) = toUpper x : xs

charToKey :: Char -> Maybe Key
charToKey '1' = Just One
charToKey '2' = Just Two
charToKey '3' = Just Three
charToKey '4' = Just Four
charToKey '5' = Just Five
charToKey '6' = Just Six
charToKey '7' = Just Seven
charToKey '8' = Just Eight
charToKey '9' = Just Nine
charToKey '0' = Just Zero
charToKey '*' = Just Star
charToKey '#' = Just Pound
charToKey _ = Nothing

parseButtonPress :: Char -> [Keypress] -> [Keypress]
parseButtonPress char [] =
  case charToKey char of
    Just parsedKey -> [(parsedKey, 1)]
    Nothing -> []
parseButtonPress char (tap@(key, presses):taps) =
  case charToKey char of
    Just parsedKey ->
      if key == parsedKey
        then (key, presses + 1) : taps
        else (parsedKey, 1) : tap : taps
    Nothing -> tap : taps

parseInput :: String -> [Keypress]
parseInput = foldr parseButtonPress []

charToTaps :: Char -> [Keypress] -> [Keypress]
charToTaps char acc =
  let charIsUpper = isUpper char
      effectiveChar = toLower char
   in case find
             (\(KeyChars key chars) -> effectiveChar `elem` chars)
             allKeyChars of
        Just (KeyChars matchedKey matchedChars) ->
          case mFindIndex matchedChars effectiveChar of
            Just index ->
              if charIsUpper
                then (Star, 1) : (matchedKey, index + 1) : acc
                else (matchedKey, index + 1) : acc
            Nothing -> acc
        Nothing -> acc

tapsToString :: [Keypress] -> String
tapsToString = go False
  where
    go _ [] = ""
    go pendingShift ((Star, presses):taps) = go (mod presses 2 == 1) taps
    go pendingShift (keypress:taps) =
      (if pendingShift
         then upperFirst
         else id)
        (keypressToString keypress) ++
      go False taps

stringToTaps :: String -> [Keypress]
stringToTaps = foldr charToTaps []

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

-- 3. how many times do digits need to be pressed for each message?
countTaps :: [Keypress] -> Presses
countTaps = foldr (\(_, presses) acc -> acc + presses) 0

totalTaps = map (countTaps . stringToTaps) convo

-- 4. What was the most popular letter. What was its cost?
sumValues _ b c = b + c

getLetterCounts char = Map.insertWithKey sumValues char 1

getMostPopularLetters :: Char -> Presses -> (String, Int) -> (String, Int)
getMostPopularLetters char count acc@(candidates, candidateCount)
  | count == candidateCount = (char : candidates, candidateCount)
  | count > candidateCount = (char : "", count)
  | count < candidateCount = acc

mostPopularLettersByCount :: String -> (String, Int)
mostPopularLettersByCount "" = ("", 0)
mostPopularLettersByCount xs =
  let charCounts = foldr getLetterCounts Map.empty (filter (`elem` alphabet) xs)
      mostPopularWithCount =
        Map.foldrWithKey getMostPopularLetters ("", 0) charCounts
   in mostPopularWithCount
-- fst.
-- mostPopular = map mostPopularLettersByCount convo
-- foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
-- 5. What was most popular letter over all?
-- What was most popular word?
