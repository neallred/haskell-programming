module Chapter12 where

import Data.Char
import Data.List

type Name = String

type Age = Integer

type ValidatePerson a = Either [PersonInvalid] a

data Person =
  Person Name
         Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  deriving (Eq, Show)

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age =
  if age < 1
    then Left [AgeTooLow]
    else Right age

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name =
  if name == ""
    then Left [NameEmpty]
    else Right name

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge

-- Chapter exercises
-- Determine the kinds
----------------------
-- 1. id :: a -> a
-- The kind of a is *
-- 2. r :: a -> f a
-- The kind of a is *
-- The kind of f is * -> *
-- String Processing
--------------------
-- 1.
theToA :: String -> String
theToA x
  | map toLower x == "the" = "a"
  | otherwise = x

replaceThe = unwords . map theToA . words

vowels = "AEIOUaeiou"

consonants = filter (not . isVowel) $ ['A' .. 'Z'] ++ ['a' .. 'z']

isVowel = flip elem vowels

isConsonant = flip elem consonants

firstIsVowel :: String -> Bool
firstIsVowel "" = False
firstIsVowel (x:xs) = isVowel x

-- 2.
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel "" = 0
countTheBeforeVowel str
  | null $ words str = 0
  | otherwise = snd $ foldr f ("", 0) (words . map toLower $ str)
  where
    f :: String -> (String, Integer) -> (String, Integer)
    f the@"the" (priorWord, count) =
      ( the
      , if firstIsVowel priorWord
          then count + 1
          else count)
    f notThe (_, count) = (notThe, count)

countTheBeforeVowelCheck =
  countTheBeforeVowel "a the the evil cow the yak the albatross the" == 2

-- 3.
countVowels :: String -> Int
countVowels = length . filter isVowel

--
-- Validate the word
validateWord :: String -> Maybe String
validateWord xs =
  let (consonants, vowels) = foldr foldCharacter (0, 0) xs
   in if vowels > consonants
        then Nothing
        else Just xs

foldCharacter char (consonantCount, vowelCount) =
  ( if isConsonant char
      then consonantCount + 1
      else consonantCount
  , if isVowel char
      then vowelCount + 1
      else vowelCount)

-- It's only Natural
data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | x == 0 = Just Zero
  | otherwise = Just (go x Zero)
  where
    go 0 acc = acc
    go positive acc = go (positive - 1) (Succ acc)

-- Small library for Maybe
-- 1.
isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust _ = False

isNothing = not . isJust

-- 2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z f Nothing = z
mayybee _ f (Just something) = f something

-- 3.
fromMaybe :: a -> Maybe a -> a
fromMaybe z Nothing = mayybee z id Nothing
fromMaybe z something = mayybee z id something

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just something) = [something]

-- 5.
catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList

-- 6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr foldMaybes (Just [])

foldMaybes :: Maybe a -> Maybe [a] -> Maybe [a]
foldMaybes Nothing _ = Nothing
foldMaybes _ Nothing = Nothing
foldMaybes (Just x) (Just xs) = Just (x : xs)

-- Small library for Either
-- 1.
getLefts :: Either a b -> [a] -> [a]
getLefts (Left x) acc = x : acc
getLefts _ acc = acc

lefts' :: [Either a b] -> [a]
lefts' = foldr getLefts []

-- 2.
getRights :: Either a b -> [b] -> [b]
getRights (Right x) acc = x : acc
getRights _ acc = acc

rights :: [Either a b] -> [b]
rights = foldr getRights []

-- 3.
groupEithers :: Either a b -> ([a], [b]) -> ([a], [b])
groupEithers (Left x) (lefts, rights) = (x : lefts, rights)
groupEithers (Right x) (lefts, rights) = (lefts, x : rights)

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr groupEithers ([], [])

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left x) = Nothing
eitherMaybe' f (Right x) = Just (f x)

-- 5.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x

-- 6.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f (Left x) = Nothing
eitherMaybe'' f (Right x) = Just (either' id f (Right x))

-- Unfolds
-- Why bother?
-- :t iterate is iterate :: (a -> a) -> a -> [a]
-- :t unfoldr is (b -> Maybe (a, b)) -> b -> [a]
-- Write your own iterate and unfoldr
-- 1.
myIterate :: (a -> a) -> a -> [a]
myIterate f currentIteration =
  let nextIteration = f currentIteration
   in currentIteration : myIterate f nextIteration

-- 2.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f currentIteration =
  case f currentIteration of
    Nothing -> []
    Just (x, y) -> x : myUnfoldr f y

-- 3.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

-- Finally something other than  list!
data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1.
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f current =
  case f current of
    Just (x1, y, x2) -> Node (unfold f x1) y (unfold f x2)
    Nothing -> Leaf

-- 2.
growUntil :: Integer -> Integer -> Maybe (Integer, Integer, Integer)
growUntil x y =
  if y > x
    then Nothing
    else Just (y + 1, y, y + 1)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (growUntil n) 0
