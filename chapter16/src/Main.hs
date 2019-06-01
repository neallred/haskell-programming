{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Char

import Data.Monoid (Product)
import Test.QuickCheck

labelDoChunk :: String -> IO ()
labelDoChunk str =
  putStrLn ("\n" ++ str ++ ":\n" ++ map (const '-') str ++ "-\n")

labelId :: String -> IO ()
labelId x = putStrLn ("testing identity law for " ++ x)

labelComp :: String -> IO ()
labelComp x = putStrLn ("testing composability law for " ++ x)

data Two a b =
  Two a b
  deriving (Eq, Show)

data Or a b
  = First' a
  | Second' b
  deriving (Eq, Show)

-- :t fmap
-- fmap :: (a -> b) -> f a -> f b
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Functor (Or a) where
  fmap f (First' x) = First' x
  fmap f (Second' x) = Second' (f x)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose g f x = ((fmap f) . (fmap g) $ x) == (fmap (f . g) x)

listFunctornessIdentity :: [Int] -> Bool
listFunctornessIdentity x = functorIdentity x

composeFunctorness = functorCompose (+ 1) (* 2)

listFunctornessCompose x = composeFunctorness (x :: [Int])

-- Exercises: Instances of Func
-- 1.
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

-- 2.
data Pair a =
  Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Pair x y)

-- 3.
instance (Monoid a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    let x = mempty
    y <- arbitrary
    return (Two x y)

-- 4.
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    return (Three mempty mempty x)

-- 5.
data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Monoid a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Three' mempty x y)

-- 6.
data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance (Monoid a, Monoid b, Monoid c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    return (Four mempty mempty mempty x)

-- 7.
data Four' a b =
  Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x1 x2 x3 y) = Four' x1 x2 x3 (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    x1 <- arbitrary
    x2 <- arbitrary
    x3 <- arbitrary
    y <- arbitrary
    return (Four' x1 x2 x3 y)

-- 8.
-- Not possible to implement a functor instnace for data Trivial = Trivial. It has the wrong kind.
-- Is `*`, but would need to be `* -> *`. There's no structure to fmap over. Just like you can't fmap over Char or Bool, and so those don't have Functor instances either
instancesOfFuncExercises :: IO ()
instancesOfFuncExercises
  -- example.
 = do
  labelDoChunk "Instances of func exercises"
  putStrLn "testing identity law for list of ints"
  quickCheck listFunctornessIdentity
  putStrLn "testing compose law for list of ints"
  quickCheck listFunctornessCompose
  -- 1.
  putStrLn "testing identity law for Identity String"
  quickCheck (functorIdentity :: Identity String -> Bool)
  putStrLn "testing compose law for Identity String"
  quickCheck
    (functorCompose (fmap toUpper) (++ "asfd") :: Identity String -> Bool)
  -- 2.
  putStrLn "testing identity law for Pair (Either Bool Int)"
  quickCheck (functorIdentity :: Pair (Either Bool Int) -> Bool)
  putStrLn "testing compose law for Pair (Either Bool Int)"
  quickCheck
    (functorCompose (fmap (* 3)) (fmap (+ 3)) :: Pair (Either Bool Int) -> Bool)
  -- 3.
  putStrLn "testing identity law for Two () (Either Bool Int)"
  quickCheck (functorIdentity :: Two () (Either Bool Int) -> Bool)
  putStrLn "testing compose law for Two String (Either Bool Int)"
  quickCheck
    (functorCompose (fmap (* 3)) (fmap (+ 3)) :: Two String (Either Bool Int) -> Bool)
  -- 4.
  putStrLn "testing identity law for Three (Product Int) (Product Int) Bool"
  quickCheck (functorIdentity :: Three (Product Int) (Product Int) Bool -> Bool)
  putStrLn "testing compose law for Three (Product Int) (Product Int) Bool"
  quickCheck
    (functorCompose (True &&) (False ||) :: (Three (Product Int) (Product Int) Bool -> Bool))
  -- 5.
  putStrLn "testing identity law for Three' (Product Int) Int"
  quickCheck (functorIdentity :: Three' (Product Int) Int -> Bool)
  putStrLn "testing compose law for Three' (Product Int) Int"
  quickCheck
    (functorCompose (* 42) (9001 -) :: (Three' (Product Int) Int -> Bool))
  -- 6.
  putStrLn "testing identity law for Four String String String String"
  quickCheck (functorIdentity :: Four String String String String -> Bool)
  putStrLn "testing compose law for Four String String String String"
  quickCheck
    (functorCompose (filter (== 'a')) (++ "asvwerwer") :: (Four String String String String -> Bool))
  -- 7.
  putStrLn "testing identity law for Four' String String"
  quickCheck (functorIdentity :: Four' String String -> Bool)
  putStrLn "testing compose law for Four' String String"
  quickCheck
    (functorCompose (filter (== 'a')) (++ "asvwerwer") :: (Four' String String -> Bool))

-- Exercise :: Possibly
data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers x) = Yeppers (f x)
  fmap _ _ = LolNope

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = frequency [(1, return LolNope), (3, fmap Yeppers arbitrary)]

possiblyExercise = do
  labelDoChunk "Possibly exercise"
  putStrLn "testing identity law for Possibly String"
  quickCheck (functorIdentity :: Possibly String -> Bool)
  putStrLn "testing compose law for Possibly String"
  quickCheck
    (functorCompose (filter (== 'a')) (map toUpper) :: (Possibly String -> Bool))

-- Exercise: Either Short Exercise
-- 1.
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second x) = Second (f x)
  fmap _ (First x) = First x

instance (Monoid a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [(1, return (First mempty)), (3, fmap Second arbitrary)]

sumExercise = do
  labelDoChunk "Maybe exercise"
  putStrLn "testing identity law for Sum String Int"
  quickCheck (functorIdentity :: Sum String Int -> Bool)
  putStrLn "testing compose law for Sum String Int"
  quickCheck (functorCompose (* 23) (* 44) :: (Sum String Int -> Bool))

-- 2.
-- You need kind * -> *
-- To get that, you have to have already applied the first type or kind argument
-- The application happens in the instance of Functor, which means you already are not
-- operating on that type
-- And even if you tried, you would be modifying structure, which breaks functor laws.
--
-- Chapter Exercises
-- Write Functor Instances
-- 1.
data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary =
    frequency
      [(1, return Finance), (8, fmap Desk arbitrary), (8, fmap Bloor arbitrary)]

-- 2.
data K a b =
  K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K x) = K x

instance (Arbitrary a, Arbitrary b) => Arbitrary (K a b) where
  arbitrary = fmap K arbitrary

-- interesting, couldn't do functorCompose on K [Int] Char or K [Int] ()
-- Because the types have to line up for the functions being composed,
-- even if in the functor instance there is NO way that the functions can ever be applied to the phantom type
-- 3.
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K' a b =
  K' a

instance Functor (Flip K' a) where
  fmap = undefined

-- 9.
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- instance (Arbitrary a) => Arbitrary (List a) where
--   arbitrary = do
--     let bob = fmap Cons arbitrary
--     frequency [(1, return Nil), (20, ((fmap Cons arbitrary) Nil))]
functorInstances :: IO ()
functorInstances = do
  labelDoChunk "More Functor Instances"
  -- 1.
  labelId "Quant Bool String"
  quickCheck (functorIdentity :: Quant Bool String -> Bool)
  labelComp "Quant Bool String"
  quickCheck (functorCompose (++ " ") tail :: Quant Bool String -> Bool)
  -- 2.
  labelId "K [Int] Char"
  quickCheck (functorIdentity :: K [Int] Char -> Bool)
  labelComp "K [Int] [Int]"
  quickCheck
    (functorCompose (fmap (+ 1)) (fmap (* 42)) :: K [Int] [Int] -> Bool)
  -- 9.
  labelId "List Int"
  -- quickCheck (functorIdentity :: List Int -> Bool)
  labelComp "List Int"
  -- quickCheck (functorCompose (* 9001) (* 42) :: List Int -> Bool)

main :: IO ()
main = do
  instancesOfFuncExercises
  possiblyExercise
  sumExercise
  functorInstances
