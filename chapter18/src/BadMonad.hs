module BadMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMeBad a =
  CountMeBad Integer a
  deriving (Eq, Show)

instance Functor CountMeBad where
  fmap f (CountMeBad i a) = CountMeBad (i + 1) (f a)

instance Applicative CountMeBad where
  pure = CountMeBad 0
  CountMeBad n f <*> CountMeBad n' a = CountMeBad (n + 1) (f a)

instance Monad CountMeBad where
  return = pure
  CountMeBad n a >>= f =
    let CountMeBad _ b = f a
     in CountMeBad (n + 1) b

instance Arbitrary a => Arbitrary (CountMeBad a) where
  arbitrary = CountMeBad <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMeBad a) where
  (=-=) = eq

data CountMe a =
  CountMe Integer a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe n' b = f a
     in CountMe (n + n') b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

main = do
  let triggerBad :: CountMeBad (Int, String, Int)
      triggerBad = undefined
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  putStrLn $ "Testing bad CountMe"
  quickBatch $ functor triggerBad
  quickBatch $ applicative triggerBad
  quickBatch $ monad triggerBad
  putStrLn $ "Testing good CountMe"
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
