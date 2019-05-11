module UsingQuickCheck where

import Data.List (isPrefixOf, isSuffixOf, sort)

-- import Data.Numbers.Primes
-- import Test.Hspec
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
    go y status@(Nothing, t) = (Just y, t)
    go y status@(Just x, t) = (Just y, x >= y)
-- propListOrdered :: (Ord a, Arbitrary a, Eq a) => [a] -> Bool
-- propListOrdered xs = listOrdered $ sort xs
-- 
-- runListOrdered = quickCheck propListOrdered
