module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n   d count
           | n < d = (count, n)
           | otherwise =
                  go (n - d) d (count + 1)

intMult :: Integral a => a -> a -> a
intMult x y = go x y 0
  where go x y count
          | y == 0 = count
          | otherwise =
              go x (y - 1) (count + x)

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` 4
    describe "dividedBy" $ do
        it "15 divded by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is\
           \ 4 remainder 2" $ do 
            dividedBy 22 5 `shouldBe` (4, 2)
    describe "intMult" $ do
        it "is zero when the first number is 0" $ do
            intMult 0 3 `shouldBe` 0
        it "is zero when the second number is 0" $ do
            intMult 9 0 `shouldBe` 0
        it "1 is identity when it comes first" $ do
            intMult 9 1 `shouldBe` 9
        it "1 is identity when it comes second" $ do
            intMult 1 9 `shouldBe` 9
        it "multiplies big numbers correctly" $ do
            intMult 99 98 `shouldBe` 9702
        it "multiplies small numbers correctly" $ do
            intMult 7 3 `shouldBe` 21
    describe "+1 property testing" $ do
        it "x + 1 is always\
              \ is greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)


genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
