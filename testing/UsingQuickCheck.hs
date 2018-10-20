module UsingQuickCheck where

import Test.QuickCheck
import Test.Hspec
import Data.List (sort, isPrefixOf, isSuffixOf)
import Data.Numbers.Primes

-- 1.
half :: Float -> Float
half x = x / 2

halfIdentity :: Float -> Float
halfIdentity = (*2) . half

runQcHalfIdentity :: IO ()
runQcHalfIdentity = quickCheck (\x -> x == halfIdentity x)

-- 2.
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)


prop_ListOrderedInt :: [Int] -> Bool
prop_ListOrderedInt x = listOrdered (sort x) == True


prop_ListOrderedBool :: [Bool] -> Bool
prop_ListOrderedBool x = listOrdered (sort x) == True

prop_ListOrderedString :: [String] -> Bool
prop_ListOrderedString x = listOrdered (sort x) == True

prop_ListOrderedChar :: [Char] -> Bool
prop_ListOrderedChar x = listOrdered (sort x) == True

runAllListOrderedQc =
  quickCheck prop_ListOrderedInt >>
  quickCheck prop_ListOrderedBool >>
  quickCheck prop_ListOrderedString >>
  quickCheck prop_ListOrderedChar

-- 3.
plusAssociative x y z =
  x + (y + z) == (x + y) + z

prop_PlusAssociative :: Int -> Int -> Int -> Bool
prop_PlusAssociative x y z = (plusAssociative x y z)

plusCommutative x y =
  x + y == y + x

prop_PlusCommutative :: Int -> Int -> Bool
prop_PlusCommutative x y = (plusCommutative x y)

runQcPlus :: IO ()
runQcPlus =
  quickCheck prop_PlusAssociative >>
  quickCheck prop_PlusCommutative

-- 4.
multAssociative x y z =
  x * (y * z) == (x * y) * z

prop_MultAssociative :: Int -> Int -> Int -> Bool
prop_MultAssociative x y z = (multAssociative x y z)

multCommutative x y =
  x * y == y * x

prop_MultCommutative :: Int -> Int -> Bool
prop_MultCommutative x y = (multCommutative x y)

runQcMult :: IO ()
runQcMult =
  quickCheck prop_MultAssociative >>
  quickCheck prop_MultCommutative


-- 5.
myQuotRem x y = (quot x y)*y + (rem x y) == x

prop_QuotRem :: Int -> Int -> Property
prop_QuotRem x y =
  x > 0 && y > 0 ==> myQuotRem x y

 

divMod x y = (div x y)*y + (mod x y) == x





























-- Thanks to https://www.fpcomplete.com/blog/2017/01/quickcheck for these examples an explanations:
prop_PrefixSuffix :: [Int] -> Int -> Bool
prop_PrefixSuffix xs n = isPrefixOf prefix xs &&
                         isSuffixOf (reverse prefix) (reverse xs)
  where prefix = take n xs


prop_Sqrt :: Double -> Bool
prop_Sqrt x
  | x < 0            = isNaN sqrtX
  | x == 0 || x == 1 = sqrtX == x
  | x < 1            = sqrtX > x
  | x > 1            = sqrtX > 1 && sqrtX < x
  where
    sqrtX = sqrt x

prop_Index_v1 :: [Integer] -> Int -> Bool
prop_Index_v1 xs n = xs !! n == head (drop n xs)

prop_Index_v2 :: (NonEmptyList Integer) -> NonNegative Int -> Bool
prop_Index_v2 (NonEmpty xs) (NonNegative n) = xs !! n == head (drop n xs)

prop_Index_v3 :: (NonEmptyList Integer) -> NonNegative Int -> Property
prop_Index_v3 (NonEmpty xs) (NonNegative n) =
  n < length xs ==> xs !! n == head (drop n xs)

prop_Index_v4 :: (NonEmptyList Integer) -> Property
prop_Index_v4 (NonEmpty xs) =
  forAll (choose (0, length xs-1)) $ \n -> xs !! n == head (drop n xs)


prop_PrimeFactors :: (Positive Int) -> Bool
prop_PrimeFactors (Positive n) = isPrime n || all isPrime (primeFactors n)

prop_PrimeSum_v1 :: Int -> Int -> Property
prop_PrimeSum_v1 p q =
  p > 2 && q > 2 && isPrime p && isPrime q ==> even (p + q)

prop_PrimeSum_v1' :: Int -> Int -> Property
prop_PrimeSum_v1' p q =
  p > 2 && q > 2 && isPrime p && isPrime q ==>
  classify (p < 20 && q < 20) "trivial" $ even (p + q)


prop_PrimeSum_v2 :: (Positive (Large Int)) -> (Positive (Large Int)) -> Property
prop_PrimeSum_v2 (Positive (Large p)) (Positive (Large q)) =
  p > 2 && q > 2 && isPrime p && isPrime q ==>
  collect (if p < q then (p, q) else (q, p)) $ even (p + q)


prop_PrimeSum_v3 :: Property
prop_PrimeSum_v3 =
  forAll (choose (1, 1000)) $ \ i ->
    forAll (choose (1, 1000)) $ \ j ->
      let (p, q) = (primes !! i, primes !! j) in
      collect (if p < q then (p, q) else (q, p)) $ even (p + q)

newtype Prime a = Prime a deriving Show

instance (Integral a, Arbitrary a) => Arbitrary (Prime a) where
  arbitrary = do
    x <- frequency [ (10, choose (0, 1000))
                   , (5, choose (1001, 10000))
                   , (1, choose (10001, 50000))
                   ]
    return $ Prime (primes !! x)


prop_PrimeSum_v4 :: Prime Int -> Prime Int -> Property
prop_PrimeSum_v4 (Prime p) (Prime q) =
  p > 2 && q > 2 ==> classify (p < 1000 || q < 1000) "has small prime" $ even (p + q)
