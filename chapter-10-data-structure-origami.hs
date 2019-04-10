module Chapter10 where

import Data.Time

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f acc [] = acc
-- foldl f acc (x:xs) = foldl f (f acc x) xs
-- f x y = conc ["(",x,"+",y,")"]
--foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldl' :: (a -> b -> b) -> b -> [a] -> b
foldl' f acc [] = acc
foldl' f acc (x:xs) = foldl' f (f x acc) xs

-- Is different than both foldl and foldr. Why?
-- λ> foldl f "0" (map show [1..5])
-- "(((((0+1)+2)+3)+4)+5)"
-- λ> foldl' f "0" (map show [1..5])
-- "(5+(4+(3+(2+(1+0)))))"
-- λ> foldr f "0" (map show [1..5])
-- "(1+(2+(3+(4+(5+0)))))"
-- Exercises: Understanding Folds
---------------------------------
-- 1.
understandingFolds1 =
  all
    ((==) 120)
    [ (foldr (*) 1 [1 .. 5])
    , (foldl (flip (*)) 1 [1 .. 5]) -- b
    , (foldl (*) 1 [1 .. 5]) -- c
    ]

-- In this case (*) is commutative, so flipping does not change anything
-- 2.
understandingFolds2a = foldl (flip (*)) 1 [1 .. 3]

understandingFolds2b = foldl (flip (*)) (1 * 1) [2 .. 3]

understandingFolds2c = foldl (flip (*)) ((1 * 1) * 2) [3]

understandingFolds2d = foldl (flip (*)) (((1 * 1) * 2) * 3) []

understandingFolds2e = (((1 * 1) * 2) * 3)

understandingFolds2f = ((1 * 2) * 3)

understandingFolds2g = (2 * 3)

understandingFolds2h = 6

-- 3.
-- One difference between foldr and foldl is c) foldr, but not foldl, associates to the right
-- 4. Folds are catamorphisms, which means they are generally used to a) reduce structure
-- 5.
understandingFolds5a = foldr (++) "" ["woot", "WOOT", "woot"]

understandingFolds5b = foldr max (minBound :: Char) "fear is the little death"

understandingFolds5c = and [False, True] -- ? doesn't need to fold?

understandingFolds5d = foldr (||) False [False, True]

understandingFolds5e = foldl (flip ((++) . show)) "" [1 .. 5]

uf5e_a = foldl (flip ((++) . show)) "" [1 .. 5]

uf5e_b = foldl (flip ((++) . show)) ((flip ((++) . show)) "" 1) [2 .. 5]

uf5e_c =
  foldl
    (flip ((++) . show))
    ((flip ((++) . show)) ((flip ((++) . show)) "" 1) 2)
    [3 .. 5]

uf5e_d =
  foldl
    (flip ((++) . show))
    ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) "" 1) 2) 3)
    [4 .. 5]

uf5e_e =
  foldl
    (flip ((++) . show))
    ((flip ((++) . show))
       ((flip ((++) . show))
          ((flip ((++) . show)) ((flip ((++) . show)) "" 1) 2)
          3)
       4)
    [5]

uf5e_f =
  (foldl
     (flip ((++) . show))
     ((flip ((++) . show))
        ((flip ((++) . show))
           ((flip ((++) . show))
              ((flip ((++) . show)) ((flip ((++) . show)) "" 1) 2)
              3)
           4)
        5)
     ([] :: [Integer]))

uf5e_g =
  ((flip ((++) . show))
     ((flip ((++) . show))
        ((flip ((++) . show))
           ((flip ((++) . show)) ((flip ((++) . show)) "" 1) 2)
           3)
        4)
     5)

uf5e_h =
  ((flip ((++) . show))
     ((flip ((++) . show))
        ((flip ((++) . show)) ((flip ((++) . show)) (((++) . show) 1 "") 2) 3)
        4)
     5)

uf5e_i =
  ((flip ((++) . show))
     ((flip ((++) . show))
        ((flip ((++) . show)) ((flip ((++) . show)) (((++) "1") "") 2) 3)
        4)
     5)

uf5e_j =
  ((flip ((++) . show))
     ((flip ((++) . show))
        ((flip ((++) . show)) ((flip ((++) . show)) ((++) "1" "") 2) 3)
        4)
     5)

uf5e_k =
  ((flip ((++) . show))
     ((flip ((++) . show))
        ((flip ((++) . show)) ((flip ((++) . show)) "1" 2) 3)
        4)
     5)

uf5e_l =
  ((flip ((++) . show))
     ((flip ((++) . show)) ((flip ((++) . show)) ((++) "2" "1") 3) 4)
     5)

uf5e_m =
  ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) "21" 3) 4) 5)

uf5e_n = ((flip ((++) . show)) ((flip ((++) . show)) ((++) "3" "21") 4) 5)

uf5e_o = ((flip ((++) . show)) ((flip ((++) . show)) "321" 4) 5)

uf5e_p = ((flip ((++) . show)) (((++) . show) 4 "321") 5)

uf5e_q = ((flip ((++) . show)) ((++) "4" "321") 5)

uf5e_r = ((flip ((++) . show)) "4321" 5)

uf5e_s = ((++) . show) 5 "4321"

uf5e_t = (++) "5" "4321"

uf5e_u = "54321"

understandingFive5E =
  all
    (== "54321")
    [ uf5e_a
    , uf5e_b
    , uf5e_c
    , uf5e_d
    , uf5e_e
    , uf5e_f
    , uf5e_g
    , uf5e_h
    , uf5e_i
    , uf5e_j
    , uf5e_k
    , uf5e_l
    , uf5e_m
    , uf5e_n
    , uf5e_o
    , uf5e_p
    , uf5e_q
    , uf5e_r
    , uf5e_s
    , uf5e_t
    , uf5e_u
    ]

-- uf5e_a = foldl (flip ((++) . show)) "" [1 .. 5]
-- uf5e_b = foldl (flip ((++) . show)) ((flip ((++) . show)) "" 1) [2 .. 5]
-- uf5e_c = foldl (flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) "" 1) 2) [3 .. 5]
-- uf5e_d = foldl (flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) "" 1) 2) 3) [4 .. 5]
-- uf5e_e = foldl (flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) "" 1) 2) 3) 4) [5]
-- uf5e_f = foldl (flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) "" 1) 2) 3) 4) 5) []
-- uf5e_g = ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) "" 1) 2) 3) 4) 5)
-- uf5e_h = ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) (((++) . show) 1 "") 2) 3) 4) 5)
-- uf5e_i = ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) (((++) "1") "") 2) 3) 4) 5)
-- uf5e_j = ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) ((++) "1" "") 2) 3) 4) 5)
-- uf5e_k = ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) "1" 2) 3) 4) 5)
-- uf5e_l = ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) ((++) "2" "1") 3) 4) 5)
-- uf5e_m = ((flip ((++) . show)) ((flip ((++) . show)) ((flip ((++) . show)) "21" 3) 4) 5)
-- uf5e_n = ((flip ((++) . show)) ((flip ((++) . show)) ((++) "3" "21") 4) 5)
-- uf5e_o = ((flip ((++) . show)) ((flip ((++) . show)) "321" 4) 5)
-- uf5e_p = ((flip ((++) . show)) (((++) . show) 4 "321") 5)
-- uf5e_q = ((flip ((++) . show)) ((++) "4" "321") 5)
-- uf5e_r = ((flip ((++) . show)) "4321" 5)
-- uf5e_s = ((++) . show) 5 "4321"
-- uf5e_t = (++) "5" "4321"
-- uf5e_u = "54321"
-- understandingFolds5f = foldr
-- uf5f_a = foldr const 'a' [1 .. 5]
-- 
-- uf5f_b = const 1 (foldr const 'a' [2 .. 5])
-- uf5f_c = const 1 (const 2 (foldr const 'a' [3 .. 5]))
-- uf5f_d = const 1 (const 2 (const 3 (foldr const 'a' [4 .. 5])))
-- uf5f_e = const 1 (const 2 (const 3 (foldr const 'a' [4 .. 5])))
-- uf5f_f = const 1 (const 2 (const 3 (const 4 (foldr const 'a' [5]))))
-- uf5f_g = const 1 (const 2 (const 3 (const 4 (const 5 (foldr const 'a' [])))))
-- uf5f_h = const 1 (const 2 (const 3 (const 4 (const 5 'a'))))
-- uf5f_i = const 1 (const 2 (const 3 (const 4 5)))
-- uf5f_j = const 1 (const 2 (const 3 4))
-- uf5f_k = const 1 (const 2 3)
-- uf5f_l = const 1 2
-- uf5f_l = 1
-- but that does not work because const is a -> b -> a
-- and foldr requires b -> a -> a
-- and foldr requires (a -> b -> b)
-- so it won't type check, and it won't run
understandingFolds5f = foldr (flip const) 'a' [1 .. 5]

-- So the right thing to think about is what types of arguments the accumulating function gets,
-- and whether it matches foldl's signature (b -> a -> b) or foldr's (a -> b -> b)
understandingFolds5g = foldr (flip const) 0 "tacos" --

-- so the foldr function expects the accumulator, based on the arguments passed to,
-- be (Char -> Num -> Num),
-- but const would normally,
-- in the first function that gets evaluated, be (const 's' 0)
-- which is (Char -> Num -> Char)
-- So if the fold is going to work at all, the accumulator should be (flip const)
--
-- foldr (a -> b -> b) b -> [a] -> b
-- foldl (a -> b -> b) -> b -> [a] -> b
-- foldl (flip const) 0 "burritos" -- similar deal going on here
understandingFolds5h = foldl const 0 "burritos" -- similar deal going on here

-- The first thing will be the inmost (and left associated) `flip const 0 'b'`, which is `const 'b' 0` Char -> Num -> Char
-- a -> b -> a
-- but foldl needs it to be b -> a -> b
-- Even if there were a way for this to evaluated, it would break the type of the function.
-- foldl (flip const)'z'[1..5]
understandingFolds5i = foldl const 'z' [1 .. 5] -- first thing evaluated is flip const 'z' 1

-- or const 1 'z',
-- Char -> Num -> Char
-- which in the context of the fold is
-- a -> b -> a
-- but fold needs b -> a -> b
-- Exercises: Database processing
data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- 1.
dbp1f :: DatabaseItem -> [UTCTime] -> [UTCTime]
dbp1f (DbDate time) times = time : times
dbp1f _ times = times

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr dbp1f []

databaseProcessing1 = filterDbDate theDatabase

-- 2.
dbp2f :: DatabaseItem -> [Integer] -> [Integer]
dbp2f (DbNumber num) nums = num : nums
dbp2f _ nums = nums

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr dbp2f []

databaseProcessing2 = filterDbNumber theDatabase

-- 3.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5.
avgDb :: [DatabaseItem] -> Double
avgDb x =
  (fromInteger (sum . filterDbNumber $ x)) /
  (fromInteger (toInteger (length (filterDbNumber x))))

-- relation between foldr and foldl for finite lists
foldr' f z xs = foldl (flip f) z (reverse xs)

-- Chpater Exercises
-- Warm-up and review
-- 1.
stops = "pbtdkg"

vowels = "aeiou"

aba a b = [(x, y, z) | x <- a, y <- b, z <- a]

stopVowelsA = aba stops vowels

stopVowelsB = filter (\(firstChar, _, _) -> firstChar == 'p') stopVowelsA

nouns =
  [ "Elm"
  , "Haskell"
  , "vacuum"
  , "carpet"
  , "schaudenfreude"
  , "avionics"
  , "jet"
  , "Bob"
  , "crabs"
  , "code"
  ]

verbs = ["jet", "run", "paint", "code", "think", "grimace", "galavant"]

stopVowelsC = aba nouns verbs

-- It finds the average word length of a string (assumes separated by spaces because words)
-- 2.
seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))

-- 3.
seekritFunc' :: (Fractional a) => String -> a
seekritFunc' x =
  fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

-- Rewriting functions using folds
-- 1.
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\a b -> f a || b) False xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr (\a b -> f a || b) False

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem x = any (== x)

myElem' :: Eq a => a -> [a] -> Bool
myElem' x xs = foldr (\a b -> b || (== x) a) False xs

-- 4.
myReverse :: [a] -> [a]
myReverse = foldr (\x -> (++ [x])) []

-- 5.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x -> ((:) (f x))) []

-- 6.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f =
  foldr
    (\x ->
       if (f x)
         then ((:) x)
         else ((++) []))
    []

-- 7.
mySquish :: [[a]] -> [a]
mySquish = foldr (++) []

-- 8.
mySquishMap :: (a -> [b]) -> [a] -> [b]
mySquishMap f = foldr (\x -> ((f x) ++)) []

-- 9.
squishAgain :: [[a]] -> [a]
squishAgain = mySquishMap (id)

-- 10.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) =
  foldr
    (\x y ->
       if f x y == GT
         then x
         else y)
    x
    xs

-- 11.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) =
  foldr
    (\x y ->
       if f x y == LT
         then x
         else y)
    x
    xs
