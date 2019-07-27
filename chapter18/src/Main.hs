module Main where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Applicative (liftA, liftA2)
import Control.Monad (join)

-- Exercise: Either Monad
data Sum' a b
  = First' a
  | Second' b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Sum' a b) where
  (<>) (First' x1) (First' x2) = First' (x1 <> x2)
  (<>) (Second' x1) (Second' x2) = Second' (x1 <> x2)
  (<>) (First' x) _ = First' x
  (<>) _ (First' x) = First' x

instance (Monoid a, Monoid b) => Monoid (Sum' a b) where
  mempty = First' mempty
  mappend = (<>)

instance Functor (Sum' a) where
  fmap f (First' x) = First' x
  fmap f (Second' x) = Second' (f x)

instance (Semigroup a) => Applicative (Sum' a) where
  pure = Second'
  (<*>) (First' x1) (First' x2) = First' (x1 <> x2)
  (<*>) (First' x1) (Second' _) = First' x1
  (<*>) (Second' _) (First' x1) = First' x1
  (<*>) (Second' f) (Second' x) = Second' (f x)

instance (Semigroup a) => Monad (Sum' a) where
  return = pure
  (>>=) (First' x) _ = First' x
  (>>=) m_a f = join $ fmap f m_a

-- Chapter Exercises
-- 1.
data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Semigroup (Nope a) where
  (<>) _ _ = NopeDotJpg

instance Monoid (Nope a) where
  mempty = NopeDotJpg
  mappend = (<>)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance (Eq a) => EqProp (Nope a) where
  (=-=) = eq

testTypeNope :: Nope (String, String, String)
testTypeNope = undefined

labelSection :: String -> IO ()
labelSection x = putStrLn ("\nTesting " ++ x ++ "\n-----------------\n")

nopeMonad = do
  labelSection "Nope a"
  quickBatch $ functor testTypeNope
  quickBatch $ applicative testTypeNope
  quickBatch $ monad testTypeNope

--2.
data PhhbbtttEither b a
  = Left' a
  | Right' b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (PhhbbtttEither b a) where
  (<>) (Left' x1) (Left' x2) = Left' (x1 <> x2)
  (<>) (Left' _) (Right' x) = Right' x
  (<>) (Right' x) (Left' _) = Right' x
  (<>) (Right' x1) (Right' x2) = Right' (x1 <> x2)

instance (Monoid a, Monoid b) => Monoid (PhhbbtttEither b a) where
  mempty = Left' mempty
  mappend = (<>)

instance Functor (PhhbbtttEither b) where
  fmap f (Right' x) = Right' x
  fmap f (Left' x) = Left' (f x)

instance (Semigroup b) => Applicative (PhhbbtttEither b) where
  pure = Left'
  (<*>) (Left' f) (Left' x) = Left' (f x)
  (<*>) (Right' x1) (Right' x2) = Right' (x1 <> x2)
  (<*>) (Left' f) (Right' x) = Right' x
  (<*>) (Right' x) (Left' y) = Right' x

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhbbtttEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return (Right' x)), (8, return (Left' y))]

instance (Semigroup b) => Monad (PhhbbtttEither b) where
  return = pure
  Right' x >>= _ = Right' x
  Left' x >>= f = f x

-- Why does this type check but that hang whe you try to execute it??
-- m_a >>= f = join $ fmap f m_a
getInt :: String -> PhhbbtttEither String Int
getInt x =
  case x of
    "0" -> Left' 0
    "1" -> Left' 1
    "2" -> Left' 2
    "3" -> Left' 3
    "4" -> Left' 4
    "5" -> Left' 5
    "6" -> Left' 6
    "7" -> Left' 7
    "8" -> Left' 8
    "9" -> Left' 9001
    x -> Right' (x ++ " is not a digit 0-9")

eitherOver9000 :: Int -> PhhbbtttEither String Int
eitherOver9000 x
  | x > 9000 = Left' x
  | otherwise = Right' ("not over 9000, meh: " ++ show x)

instance (Eq b, Eq a) => EqProp (PhhbbtttEither b a) where
  (=-=) = eq

testTypePhhbbtttEither ::
     PhhbbtttEither (String, Product Int, String) (String, Product Int, String)
testTypePhhbbtttEither = undefined

phhbbtttEitherMonad = do
  labelSection "PhhbbtttEither b a"
  putStrLn "Sanity test"
  print $ getInt "3" >>= eitherOver9000
  quickBatch $ functor testTypePhhbbtttEither
  quickBatch $ applicative testTypePhhbbtttEither
  quickBatch $ monad testTypePhhbbtttEither

-- 3. 
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

testTypeIdentity :: Identity (String, Product Int, String)
testTypeIdentity = undefined

identityMonad = do
  labelSection "Identity a"
  quickBatch $ functor testTypeIdentity
  quickBatch $ applicative testTypeIdentity
  quickBatch $ monad testTypeIdentity

-- 4.
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

concatList :: List a -> List a -> List a
concatList Nil Nil = Nil
concatList Nil xs = xs
concatList xs Nil = xs
concatList (Cons x Nil) ys = Cons x ys
concatList (Cons x xs) ys = Cons x (concatList xs ys)

instance Applicative List where
  pure f = Cons f Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> xs =
    let cL = concatList
     in fmap f xs `cL` (fs <*> xs)

gimmeThree :: a -> List a
gimmeThree x = Cons x (Cons x (Cons x Nil))

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= f =
    let cL = concatList
     in f x `cL` (xs >>= f)

--  Why does this type check?
--  xs >>= f = join (fmap f xs)
instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary =
    frequency [(1, return Nil), (6, (return Cons) <*> arbitrary <*> arbitrary)]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

testTypeList :: List (String, Product Int, String)
testTypeList = undefined

listMonad = do
  labelSection "List a"
  putStrLn "Sanity test"
  let bob = Cons 1 (Cons 2 (Cons 3 Nil)) >>= gimmeThree
  print bob
  quickBatch $ functor testTypeList
  quickBatch $ applicative testTypeList
  quickBatch $ monad testTypeList

main :: IO ()
main = do
  nopeMonad
  phhbbtttEitherMonad
  identityMonad
  listMonad

-- Implement methods of Monad and Functor
-- 1.
j :: Monad m => m (m a) -> m a
j = join

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f = (return (:)) <*> (f x) <*> (meh xs f)

-- 6.
flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id
