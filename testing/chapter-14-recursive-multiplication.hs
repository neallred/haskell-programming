module Chapter14RecursiveMultiplication where

import Test.Hspec

recursiveMultiplication :: (Ord a, Eq a, Num a) => a -> a -> a
recursiveMultiplication first second = go first second 0
  where
    go x y count
      | y < 1 = count + (x * y)
      | otherwise = go x (y - 1) count + x

main :: IO ()
main =
  hspec $ do
    describe "recursiveMultiplication" $ do
      it "9001 * 0 is not over 9000" $ do
        recursiveMultiplication 9001 0 `shouldBe` (0 :: Int)
      it "1 * 234 is 234" $ do
        recursiveMultiplication 1 234 `shouldBe` (234 :: Int)
      it "22 * 4 is 88" $ do recursiveMultiplication 22 4 `shouldBe` (88 :: Int)
      it "is zero when the first number is 0" $ do
        recursiveMultiplication 0 3 `shouldBe` 0
      it "is zero when the second number is 0" $ do
        recursiveMultiplication 9 0 `shouldBe` 0
      it "1 is identity when it comes first" $ do
        recursiveMultiplication 9 1 `shouldBe` 9
      it "1 is identity when it comes second" $ do
        recursiveMultiplication 1 9 `shouldBe` 9
      it "multiplies big numbers correctly" $ do
        recursiveMultiplication 99 98 `shouldBe` 9702
      it "multiplies small numbers correctly" $ do
        recursiveMultiplication 7 3 `shouldBe` 21
