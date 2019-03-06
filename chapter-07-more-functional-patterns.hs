--
-- Exercises: Grab Bag
-- 1.
mTh x y z = x * y * z

mTh' x y = \z -> x * y * z

mTh'' x = \y -> \z -> x * y * z

mTh''' = \x -> \y -> \z -> x * y * z

-- They are all equivalent.
-- It's like doing λabc.abc
-- or λaλbλc.abc
-- 2.
-- mTh 3 :: Num a => a -> a -> a (d)
-- 3.
-- a.
addOneIfOdd =
  \n ->
    case odd n of
      True -> f n
      False -> n
  where
    f n = n + 1

-- b.
addFive =
  \x ->
    \y ->
      (if x > y
         then y
         else x) +
      5

-- c.
mFlip f x y = f y x

newtype Username =
  Username String

newtype AccountNumber =
  AccountNumber Integer

data User
  = UnregisteredUser
  | RegisteredUser Username
                   AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name) (AccountNumber acctNum)) =
  putStrLn $ name ++ " " ++ show acctNum

data WherePenguinsLive
  = Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica Galapagos = False
isSouthAfrica Antarctica = False
isSouthAfrica Australia = False
isSouthAfrica SouthAmerica = False

isSouthAfrica' :: WherePenguinsLive -> Bool
isSouthAfrica' SouthAfrica = True
isSouthAfrica' _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt = Peng SouthAmerica

gentoo = Peng Antarctica

macaroni = Peng Antarctica

little = Peng Australia

galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _ = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p = (galapagosPenguin p) || (antarcticPenguin p)

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

-- Exercises: Variety Pack
-- 1.
-- a) :t k :: (a, b) -> a
-- b) :t k2 :: String. Different than k1 and k3 , which are (Num a => a)
-- I guess they get defaulted to Integer by GCHI
-- c) k3 (relabeled varietyPackK3)
varietyPackK (x, y) = x

varietyPackK1 = varietyPackK ((4 - 1), 10)

varietyPackK2 = varietyPackK ("three", (1 + 2))

varietyPackK3 = varietyPackK (3, True)

-- 2.
varietyPackF :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
varietyPackF (u, v, w) (x, y, z) = ((u, x), (w, z))

-- Exercises: Case Practice
-- 1. 
-- functionC x y = if (x > y) then x else y
functionC x y =
  case x > y of
    True -> x
    False -> y

-- 2. 
-- ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2 n =
  case even n of
    True -> (n + 2)
    False -> n

-- 3.
nums x =
  case compare x 0 of
    LT -> -1
    EQ -> 0
    GT -> 1

data Employee
  = Coder
  | Manager
  | Veep
  | CEO
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' =
  case compare e e' of
    GT -> reportBoss e e'
    EQ ->
      putStrLn
        "Neither employee\
                   \ is the boss"
    LT -> (flip reportBoss) e e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

employeeRank' ::
     (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank' f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ ->
      putStrLn
        "Neither employee\
                   \ is the boss"
    LT -> (flip reportBoss) e e'

dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: (Num a) => a -> a
oneIsOne = dodgy 1

oneIsTwo :: (Num a) => a -> a
oneIsTwo = (flip dodgy) 2

dodgyOne = dodgy 1 0 == 1

dodgyTwo = dodgy 1 1 == 11

dodgyThree = dodgy 2 2 == 22

dodgyFour = dodgy 1 2 == 21

dodgyFive = dodgy 2 1 == 12

dodgySix = oneIsOne 1 == 11

dodgySeven = oneIsOne 2 == 21

dodgyEight = oneIsTwo 1 == 21

dodgyNine = oneIsTwo 2 == 22

dodgyTen = oneIsOne 3 == 31

dodgyEleven = oneIsTwo 3 == 23

allDone =
  foldl
    (\acc curr -> acc && curr)
    True
    [ dodgyOne
    , dodgyTwo
    , dodgyThree
    , dodgyFour
    , dodgyFive
    , dodgySix
    , dodgySeven
    , dodgyEight
    , dodgyNine
    , dodgyTen
    , dodgyEleven
    ]

bloodNa :: Integer -> String
bloodNa x
  | x < 135 = "too low"
  | x > 145 = "too high"
  | otherwise = "just right"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight x y z
  | (x ^ 2) + (y ^ 2) == (z ^ 2) = "IS RIGHT"
  | otherwise = "not"

dogYrs :: Integer -> Integer
dogYrs x
  | x <= 0 = 0
  | x <= 1 = x * 15
  | x <= 2 = x * 12
  | x <= 4 = x * 8
  | otherwise = x * 6

-- Exercises: Guard Duty
-- 1.
avgGradeOne :: (Fractional a, Ord a) => a -> Char
avgGradeOne x
  | otherwise = 'F'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  where
    y = x / 100

-- 2.
avgGradeTwo :: (Fractional a, Ord a) => a -> Char
avgGradeTwo x
  | y >= 0.7 = 'C'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where
    y = x / 100

-- 3.
palThree :: (Eq a) => [a] -> Bool
palThree xs
  | xs == reverse xs = True
  | otherwise = False

-- b) True when xs is a palindrome
-- 4. a list of items that have instances of the Eq typeclass
-- 5. see above for type
-- 6. It returns c), an indication of whether its argument is a positive or negative number or zero
-- 7. numbers takes arguments that have an instance of the Num and Ord type classes
-- 8. numbers :: (Num a, Ord a) -> a -> a
numbers :: (Num a, Ord a) => a -> a
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

-- It claims _ is not matched. Why?
arith2Add :: Int -> Int -> Int
arith2Add x y = x + y

arith2AddPF :: Int -> Int -> Int
arith2AddPF = (+)

arith2AddOne :: Int -> Int
arith2AddOne x = (+) 1 x

arith2AddOnePF :: Int -> Int
arith2AddOnePF = (+ 1)

print' = putStrLn . show

arith2Main :: IO ()
arith2Main = do
  print' (0 :: Int) -- 0
  print' (arith2Add 1 0) -- 1
  print' (arith2AddOne 0) -- 1
  print' (arith2AddOnePF 0) -- 1
  print' ((arith2AddOne . arith2AddOne) 0) -- (\x -> 1 + ( 1 + x)) 0 == 2
  print' ((arith2AddOnePF . arith2AddOne) 0) -- (\x -> (+) 1 (1 + x ) == 2 
  print' ((arith2AddOne . arith2AddOnePF) 0) -- (\x -> (1 + ((+1) x)) 0 == 2
  print' ((arith2AddOnePF . arith2AddOnePF) 0) -- \(x -> (+1) ((+1) x)) 0 == 2
  print' (negate (arith2AddOne 0)) -- negate ((+1) 0) == -1
  print' ((negate . arith2AddOne) 0) -- (\x -> negate ((+1) x)) 0 == -1
  print'
    ((arith2AddOne . arith2AddOne . arith2AddOne . negate . arith2AddOne) 0)
  -- (\x -> (1 + (1 + (1 + (negate (1 + x)))))) 0 == 2

-- Chapter exercises
-- Multiple choice
-- 1. d) A polymorphic function may resolve to values of different types, depending on inputs
-- 2. b) Char -> [String]
-- 3. d) (Ord a, Num a) => a -> Bool
-- 4. b) is a higher-order function
-- 5. a) f True :: Bool
-- Let's write code
-------------------
-- 1.
tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = x `div` 10
    d = xLast `mod` 10

-- a)
tensDigit' :: Integral a => a -> a
tensDigit' x = (flip mod) 10 . fst $ divMod x 10

-- b) yes
hundreds :: Integral a => a -> a
hundreds x = (flip mod) 100 . fst $ divMod x 100

-- c)
-- Revisit this one
-- 2.
foldBoolPattern :: a -> a -> Bool -> a
foldBoolPattern x _ False = x
foldBoolPattern _ y True = y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y bool
  | not bool = x
  | bool = y

--
writeCode3g :: (a -> b) -> (a, c) -> (b, c)
writeCode3g f (x, y) = (f x, y)

writeCode4RoundTrip :: (Show a, Read a) => a -> a
writeCode4RoundTrip a = read (show a)

-- It works because the showable thing gets converted to the string.
-- And the string 
-- For this (Num a) => a type, roundTrip is an identity function.
-- Are there data types that have instances of Show and Read that do not combine to the identity function
writeCode4Main = do
  print (writeCode4RoundTrip 4)
  print (id 4)

writeCode5RoundTrip :: (Show a, Read a) => a -> a
writeCode5RoundTrip = read . show

-- 6.
writeCode6RoundTrip :: (Show a, Read b) => a -> b
writeCode6RoundTrip = read . show

-- print $ ((writeCode6RoundTrip 3) :: Int)
-- Chapter definitions
data SumOfThree a b c
  = FirstPossible a
  | SecondPossible b
  | ThirdPossible c
  deriving (Eq, Show)

sumToInt :: SumOfThree a b c -> Integer
sumToInt (FirstPossible _) = 0
sumToInt (SecondPossible _) = 1
sumToInt (ThirdPossible _) = 2

sumToInt' :: SumOfThree a b c -> Integer
sumToInt' (FirstPossible _) = 0
sumToInt' _ = 1
