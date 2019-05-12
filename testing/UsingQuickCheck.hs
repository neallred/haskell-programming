module UsingQuickCheck where

import Data.Char (toUpper)

import Data.List (sort)

import Test.QuickCheck

-- 1.
half :: (Eq a, Fractional a, Arbitrary a) => a -> a
half x = x / 2

halfIdentity :: (Eq a, Fractional a, Arbitrary a) => a -> a
halfIdentity = (* 2) . half

propHalfsies :: Double -> Bool
propHalfsies x = x == halfIdentity x

runPropHalfsies :: IO ()
runPropHalfsies = quickCheck propHalfsies

-- 2.
listOrdered :: (Ord a, Arbitrary a, Eq a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, _) = (Just y, x >= y)

propListOrderedInt :: [Int] -> Bool
propListOrderedInt xs = listOrdered $ sort xs

propListOrderedBool :: [Bool] -> Bool
propListOrderedBool xs = listOrdered $ sort xs

propListOrderedChar :: String -> Bool
propListOrderedChar xs = listOrdered $ sort xs

propListOrderedString :: [String] -> Bool
propListOrderedString xs = listOrdered $ sort xs

propListOrderedFloat :: [Float] -> Bool
propListOrderedFloat xs = listOrdered $ sort xs

runListOrderedInt :: IO ()
runListOrderedInt = quickCheck propListOrderedInt

runListOrderedBool :: IO ()
runListOrderedBool = quickCheck propListOrderedBool

runListOrderedChar :: IO ()
runListOrderedChar = quickCheck propListOrderedChar

runListOrderedString :: IO ()
runListOrderedString = quickCheck propListOrderedString

runListOrderedFloat :: IO ()
runListOrderedFloat = quickCheck propListOrderedFloat

-- 3.
plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

runPlusAssociative :: IO ()
runPlusAssociative = quickCheck plusAssociative

plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x

runPlusCommutative :: IO ()
runPlusCommutative = quickCheck plusCommutative

-- 4.
multAssociative :: Int -> Int -> Int -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

runMultAssociative :: IO ()
runMultAssociative = quickCheck multAssociative

multCommutative :: Int -> Int -> Bool
multCommutative x y = x * y == y * x

runMultCommutative :: IO ()
runMultCommutative = quickCheck multCommutative

-- 5.
propQuotRem :: Int -> Int -> Bool
propQuotRem x y = (quot x y) * y + (rem x y) == x

propertyQuotRem :: Int -> Int -> Property
propertyQuotRem x y = x /= 0 && y /= 0 ==> propQuotRem x y

runQuotRem :: IO ()
runQuotRem = quickCheck propertyQuotRem

propDivMod :: Int -> Int -> Bool
propDivMod x y = (div x y) * y + (mod x y) == x

propertyDivMod :: Int -> Int -> Property
propertyDivMod x y = x /= 0 && y /= 0 ==> propDivMod x y

runDivMod :: IO ()
runDivMod = quickCheck propertyDivMod

-- 6.
-- Neither associative nor commutative
expAssociative :: Int -> Int -> Int -> Bool
expAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

runExpAssociative :: IO ()
runExpAssociative = quickCheck expAssociative

expCommutative :: Int -> Int -> Bool
expCommutative x y = x ^ y == y ^ x

runExpCommutative :: IO ()
runExpCommutative = quickCheck expCommutative

-- 7.
testReverseReverse :: IO ()
testReverseReverse =
  quickCheck (\xs -> (reverse . reverse $ xs) == (xs :: [Int]))

-- 8.
propParenthetical :: (Eq a) => (a -> a -> a) -> a -> a -> a -> Bool
propParenthetical f x y z = (f (f x y) z) == (f z $ f x y)

propParentheticalAddition :: Int -> Int -> Int -> Bool
propParentheticalAddition = propParenthetical (+)

testParenthicalness :: IO ()
testParenthicalness = quickCheck propParentheticalAddition

propComposing :: (Eq a, Eq b, Eq c) => (b -> c) -> (a -> b) -> a -> Bool
propComposing f g x = f (g x) == (f $ g x)

propComposingInt :: Int -> Bool
propComposingInt x = propComposing (+ 2) (+ 9) x

propComposingBool :: Bool -> Bool
propComposingBool x = propComposing (== True) (== False) x

runTestComposingInt :: IO ()
runTestComposingInt = quickCheck propComposingInt

runTestComposingBool :: IO ()
runTestComposingBool = quickCheck propComposingBool

-- 9.
propFoldyA :: [(String, String)] -> [(String, String)] -> Bool
propFoldyA xs ys = foldr (:) ys xs == (++) xs ys

runPropFoldyA :: IO ()
runPropFoldyA = quickCheck propFoldyA

-- yep
propFoldyB :: [String] -> Bool
propFoldyB xs = foldr (++) [] xs == concat xs

runPropFoldyB :: IO ()
runPropFoldyB = quickCheck propFoldyB

-- yep
-- 10.
badListLengthProp :: Int -> [a] -> Bool
badListLengthProp n xs = length (take n xs) == n

badListLengthPropIntStrings :: Int -> [String] -> Property
badListLengthPropIntStrings n xs = n > 0 ==> badListLengthProp n xs

runBadListLengthProp :: IO ()
runBadListLengthProp = quickCheck badListLengthPropIntStrings

-- 11.
propReadAndShow :: (Eq a, Read a, Show a) => a -> Bool
propReadAndShow x = (read (show x)) == x

propReadAndShowBool :: Bool -> Bool
propReadAndShowBool = propReadAndShow

propReadAndShowListFloat :: [Float] -> Bool
propReadAndShowListFloat = propReadAndShow

runPropReadAndShowBool :: IO ()
runPropReadAndShowBool = quickCheck propReadAndShowBool

runPropReadAndShowListFloat :: IO ()
runPropReadAndShowListFloat = quickCheck propReadAndShowListFloat

-- Failure
square :: (Num a) => a -> a
square x = x * x

-- sqrt must take a floating
-- which are floats and doubles
-- Precision is lost in doing this, so it won't always equals
squareIdentity :: Float -> Float
squareIdentity = square . sqrt

propSquareIdentity :: Float -> Bool
propSquareIdentity x = squareIdentity x == x

runPropSquareIdentity :: IO ()
runPropSquareIdentity = quickCheck propSquareIdentity

-- Fails for these [-1.2442006, 0.5, -0.1]
-- Idempotence
twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

-- 1.
capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

capitalCapitan :: String -> Bool
capitalCapitan x =
  (capitalizeWord x == twice capitalizeWord x) &&
  (capitalizeWord x == fourTimes capitalizeWord x)

propertyCapitalize :: IO ()
propertyCapitalize = quickCheck capitalCapitan

-- 2.
sortsAlot :: [String] -> Bool
sortsAlot x =
  (sort x == twice sort x) && (sort x == fourTimes sort (x :: [String]))

propertySortsAlot :: IO ()
propertySortsAlot = quickCheck sortsAlot

-- Make a Gen random generator for the datatype
-- 1.
data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = elements [Fulse, Frue]

foolGenWeighted :: Gen Fool
foolGenWeighted = frequency [(2, return Fulse), (1, return Frue)]

instance Arbitrary Fool where
  arbitrary = foolGen
-- Hangman testing
