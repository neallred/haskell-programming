import           Data.List (intercalate)

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
                   True  -> Right age
                   False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
                   True  -> Right name
                   False -> Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age =
    mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name
          -> ValidatePerson Age
          -> ValidatePerson Person

mkPerson' (Right nameOk) (Right ageOk) =
    Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) =
    Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge


data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

-- String Processing
--------------------

-- 1.

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x     = Just x

whatThe :: Maybe String -> String
whatThe Nothing  = "a"
whatThe (Just x) = x

theWords = "The the quick brown fox jumped over the lazy dogs a man the a threarah."

replaceThe :: String -> String
replaceThe x =
   ( intercalate " "
   . (map whatThe)
   . (map notThe)
   . words
   ) x

-- 2.

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = (goCountTheBeforeVowel 0) . (words)

goCountTheBeforeVowel :: Integer -> [String] -> Integer
goCountTheBeforeVowel c [] = c
goCountTheBeforeVowel c (x1:[]) = c
goCountTheBeforeVowel c (x1:x2:xs)
    | x1 == "the" && elem (head x2) "AEIOUaeiou" = goCountTheBeforeVowel (c + 1) (x2:xs)
    | otherwise = goCountTheBeforeVowel c (x2:xs)

countVowels :: String -> Integer
countVowels = goCountVowels 0

goCountVowels :: Integer -> String -> Integer
goCountVowels x ""     = x
goCountVowels x (y:ys) = goCountVowels (x + if elem y "AEIOUaeiou" then 1 else 0) ys

-- Validate the Word
--------------------

newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels = "AEIOUaeiou"

mkWord :: String -> Maybe Word'
mkWord xs =
    let
        counts = goMkWord (0,0) xs
    in
        if (fst counts) <= (snd counts)
           then Just $ Word' xs
           else Nothing

goMkWord :: (Int, Int) -> String -> (Int, Int)
goMkWord vc "" = vc
goMkWord (v,c) (x:xs) =
    let
        isVowel = elem x vowels
    in
        goMkWord (v + (if isVowel then 1 else 0) , c + (if isVowel then 0 else 1)) xs

-- It's only Natural
--------------------

data Nat
    = Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat i
    | i > 0     = Just (goIntegerToNat i)
    | i == 0    = Just Zero
    | otherwise = Nothing

goIntegerToNat :: Integer -> Nat
goIntegerToNat 0 = Zero
goIntegerToNat i = Succ (goIntegerToNat (i - 1))

-- Small library for Maybe
--------------------------

-- 1.

isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust _        = False

isNothing = not . isJust

-- 2.

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just y) = f y
mayybee x _ Nothing  = x

-- 3.

fromMaybe :: a -> Maybe a -> a
fromMaybe x (Just y) = mayybee x (id) (Just y)
fromMaybe x Nothing  = mayybee x (id) Nothing

-- 4.

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing  = []

-- 5.

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) =
    case x of
         Just x  -> x : (catMaybes xs)
         Nothing -> (catMaybes xs)

-- 6.

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe orig@(x:xs) =
    if length (filter isNothing orig) > 0
       then
           Nothing
       else
           case x of
                 (Just x) -> Just (map (fromMaybe x) orig)
                 _        -> Nothing -- Can't happen due to if statement above

-- Small library for Either
---------------------------

-- 1.

-- lefts' :: [Either a b] -> [a]
-- lefts' [] = []
-- lefts' (x:xs) =
--     case x of
--          Left x  -> x : (lefts' xs)
--          Right x -> (lefts' xs)

lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' xs =
    foldr goLeft [] xs

goLeft :: Either a b -> [a] -> [a]
goLeft x others =
    case x of
        Left x  -> x : others
        Right x -> others

-- 2.

rights' :: [Either a b] -> [b]
rights' [] = []
rights' xs =
    foldr goRight [] xs

goRight :: Either a b -> [b] -> [b]
goRight x others =
    case x of
        Left x  -> others
        Right x -> x : others

-- 3.

partitionEithers' :: [Either a b] ->  ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

-- 4.

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left x)  = Nothing
eitherMaybe' f (Right y) = Just (f y)

-- 5.

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' f _ (Left x)  = f x
either' _ g (Right y) = g y

-- 6.

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x  = either' (\_ -> Nothing) (\y -> Just (f y)) x

-- Unfolds
----------

-- 1.

myIterate :: (a -> a) -> a -> [a]
myIterate f x = (f x) : myIterate f (f x)


-- 2.

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
    case (f x) of
         Just (y, z) -> y : myUnfoldr f z
         Nothing     -> []

-- 3.

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x

-- Finally something other than a list!
---------------------------------------

data BinaryTree a
    = Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

-- 1.

unfoldBinaryTree :: (a -> Maybe (a,b,a))
                 -> a
                 -> BinaryTree b
unfoldBinaryTree f x =
    case (f x) of
         Just (y1, z, y2) -> Node (unfoldBinaryTree f y1) z (unfoldBinaryTree f y2)
         Nothing          -> Leaf

-- 2.

-- build that tree!
treeBeard :: Integer -> BinaryTree Integer
treeBeard x = unfoldBinaryTree (entWater x) 0

-- GrOw function for building treeBeard
entWater :: Integer -> Integer -> Maybe (Integer,Integer,Integer)
entWater x y =
    case x > y of
         True  -> Just (y + 1, y, y + 1)
         False -> Nothing
