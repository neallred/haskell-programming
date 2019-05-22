module Main where

import Control.Monad
import Data.List.NonEmpty
import Data.Monoid as M

-- (Monoid, Product, Sum, mappend, mempty)
import Data.Semigroup (Semigroup, (<>))
import Test.QuickCheck

-- import Test.QuickCheck.Gen
-- import Test.QuickCheck.Property
main :: IO ()
main = do
  putStrLn "Testing monoidal properties:"
  putStrLn "Monoid assodiativity for string:"
  monoidTestStr
  putStrLn "Monoid left identity:"
  quickCheck (monoidLeftIdentity :: String -> Bool)
  putStrLn "Monoid right identity:"
  quickCheck (monoidRightIdentity :: String -> Bool)
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity
  quickCheck (ma :: BullMappend)
  quickCheck (mli :: Bull -> Bool)
  quickCheck (mri :: Bull -> Bool)
  putStrLn "Testing Monoid for Optional type:"
  putStrLn "testing mappend:"
  quickCheck
    (monoidAssoc :: First' String -> First' String -> First' String -> Bool)
  putStrLn "testing left identity:"
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  putStrLn "testing right identity:"
  quickCheck (monoidRightIdentity :: First' String -> Bool)
  semigroupExercises

-- asc :: Eq a => (a -> a -> a) -> a -> a -> Bool
-- asc (<>) a b c = a <> (b <> c) == (a <> b) <> c
monoidAssoc :: (Eq m, Semigroup m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidTestStr = quickCheck (monoidAssoc :: String -> String -> String -> Bool)

monoidTestStr2 =
  verboseCheck (monoidAssoc :: String -> String -> String -> Bool)

monoidLeftIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

-- Exercise: Maybe another Monoid
data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

newtype First' a =
  First'
    { getFirst' :: Optional a
    }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' (Only x)) (First' (Only y)) = First' (Only x)
  (<>) (First' (Only x)) (First' Nada) = First' (Only x)
  (<>) (First' Nada) (First' (Only x)) = First' (Only x)
  (<>) (First' Nada) (First' Nada) = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada
  mappend = (<>)

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary =
    frequency [(1, return (First' Nada)), (4, fmap (First' . Only) arbitrary)]

-- Chapter exercises
-- Semigroup exercises
-- 1.
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- 2.
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup (Identity a) where
  (<>) (Identity a) (Identity b) = Identity a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

-- 3.
data Two a b =
  Two a b
  deriving (Eq, Show)

pairToTwo :: (a, b) -> Two a b
pairToTwo (x, y) = Two x y

instance Semigroup (Two a b) where
  (<>) (Two x1 _) (Two _ y2) = Two x1 y2

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  x <- arbitrary
  y <- arbitrary
  return (Two x y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

-- 4.
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Semigroup (Three a b c) where
  (<>) (Three _ _ c1) (Three a2 b2 _) = Three a2 b2 c1

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return (Three x y z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = threeGen

-- 5.
data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance Semigroup (Four a b c d) where
  (<>) (Four _ _ c1 d1) (Four a2 b2 _ _) = Four a2 b2 c1 d1

fourGen ::
     (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  w <- arbitrary
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return (Four w x y z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = fourGen

type WowThatsLong
   = Four String Int [[String]] (Char, Bool) -> Four String Int [[String]] ( Char
                                                                           , Bool) -> Four String Int [[String]] ( Char
                                                                                                                 , Bool) -> Bool

-- 6.
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj False) _ = BoolConj False
  (<>) _ (BoolConj False) = BoolConj False
  (<>) _ _ = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary
    return (BoolConj x)

-- 7.
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj True) _ = BoolDisj True
  (<>) _ (BoolDisj True) = BoolDisj True
  (<>) _ _ = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary
    return (BoolDisj x)

-- 8.
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) (Snd x) _ = Snd x
  (<>) _ (Snd y) = Snd y
  (<>) (Fst x) (Fst y) = Fst y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return (Fst x)), (1, return (Snd y))]

type OrInts = Or Int Int

type OrIntsTest = OrInts -> OrInts -> OrInts -> Bool

-- 9.
newtype Combine a b =
  Combine
    { unCombine :: (a -> b)
    }

-- one function a -> a
-- one function a -> b
instance Semigroup (Combine (a -> a) (a -> b)) where
  (<>) funF funG = funF . funG

f = Combine $ \n -> M.Sum (n + 1)

g = Combine $ \n -> M.Sum (n - 1)

semigroupExercises :: IO ()
semigroupExercises = do
  putStrLn "Checking Trival semigroup property:"
  quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  putStrLn "Checking Identity a semigroup property:"
  quickCheck
    (semigroupAssoc :: Identity String -> Identity String -> Identity String -> Bool)
  putStrLn "Checking Two a b semigroup property:"
  quickCheck
    (semigroupAssoc :: Two String Int -> Two String Int -> Two String Int -> Bool)
  putStrLn "Checking Three a b c semigroup property:"
  quickCheck
    (semigroupAssoc :: Three String Int [[String]] -> Three String Int [[String]] -> Three String Int [[String]] -> Bool)
  putStrLn "Checking BoolConj semigroup property:"
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  putStrLn "Checking BoolDisj semigroup property:"
  quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  putStrLn "Checking Or a b semigroup property:"
  quickCheck (semigroupAssoc :: OrIntsTest)
