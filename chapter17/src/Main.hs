module Chapter17 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

sectionLabel x = putStrLn $ "--- " ++ x ++ " ---"

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance (Semigroup a) => Semigroup (List a) where
  (<>) = append

instance (Semigroup a) => Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a x) = Cons (f a) (fmap f x)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil Nil = Nil
  (<*>) Nil (Cons x xs) = Nil
  (<*>) (Cons x xs) Nil = Nil
  (<*>) (Cons f fs) (Cons x xs) =
    Cons (f x) (fmap f xs `append` (<*>) fs (Cons x xs))

-- Thanks much https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
myList :: Arbitrary a => Gen (List a)
myList = frequency [(1, return Nil), (20, Cons <$> arbitrary <*> myList)]

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = myList

instance (Arbitrary xs) => Arbitrary (ZipList' xs) where
  arbitrary = fmap ZipList' myList

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' num xs
  | num <= 0 = Nil
  | otherwise =
    case xs of
      Nil -> Nil
      _ ->
        let (Cons x rest) = xs
         in Cons x (take' (num - 1) rest)

toList :: [a] -> List a
toList = foldr Cons Nil

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take' 3000 l
      ys' =
        let (ZipList' l) = ys
         in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

zipWithList :: (a -> b -> c) -> List a -> List b -> List c
zipWithList _ _ Nil = Nil
zipWithList _ Nil _ = Nil
zipWithList f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWithList f xs ys)

instance (Monoid list) => Semigroup (ZipList' list) where
  (<>) = liftA2 mappend

instance (Monoid list) => Monoid (ZipList' list) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Applicative ZipList' where
  pure x = ZipList' (repeat' x)
  (<*>) (ZipList' fs) (ZipList' ys) = ZipList' (zipWithList ($) fs ys)

test1 = Right 1 == (pure 1 :: Either String Int)

test2 = (Right (+ 1) <*> Right 1) == (Right 2 :: Either String Int)

test3 = (Right (+ 1) <*> Left ":(") == (Left ":(" :: Either String Int)

test4 = (Left ":(" <*> Left "sadface.png") == (Left ":(" :: Either String Int)

data Validation err a
  = Failure' err
  | Success' a
  deriving (Eq, Show)

instance (Semigroup e) => Semigroup (Validation e a) where
  (<>) (Failure' err) (Success' a) = Failure' err
  (<>) (Success' a) (Failure' err) = Failure' err
  (<>) (Failure' err1) (Failure' err2) = Failure' (err1 <> err2)
  (<>) (Success' a) (Success' b) = Success' a

instance (Monoid e) => Monoid (Validation e a) where
  mempty = Failure' mempty
  mappend = (<>)

instance (Monoid e, Arbitrary e, Arbitrary a) =>
         Arbitrary (Validation e a) where
  arbitrary =
    frequency [(1, fmap Failure' arbitrary), (4, fmap Success' arbitrary)]

instance Functor (Validation e) where
  fmap f (Failure' x) = Failure' x
  fmap f (Success' x) = Success' (f x)

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (<*>) (Failure' err) (Success' a) = Failure' err
  (<*>) (Success' a) (Failure' err) = Failure' err
  (<*>) (Failure' err1) (Failure' err2) = Failure' (err1 <> err2)
  (<*>) (Success' f) (Success' a) = Success' (f a)

validToEither :: Validation e a -> Either e a
validToEither (Failure' err) = Left err
validToEither (Success' a) = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left e) = Failure' e
eitherToValid (Right e) = Success' e

instance (Eq a, Eq b) => EqProp (Validation a b) where
  (=-=) = eq

-- composeTestA = eitherToValid . validToEither == id
-- composeTestB = validToEither . eitherToValid == id
data Errors
  = DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

success = (Success' (+ 1) :: Validation String (Int -> Int)) <*> Success' 1

testVal1 = success == Success' 2

failure = Success' (+ 1) <*> Failure' [StackOverflow]

failure' = Failure' [MooglesChewedWires] :: Validation [Errors] Int

failures = Failure' [MooglesChewedWires] <*> Failure' [StackOverflow]

testVal2 = failure' == Failure' [MooglesChewedWires, StackOverflow]

listApplicativeExercise :: IO ()
listApplicativeExercise = do
  sectionLabel "List Applicative Exercise"
  let bob = Cons 3 Nil <> Cons 4 Nil :: List (Sum Int)
  print bob
  quickBatch $ applicative (Cons (2 :: Sum Int, "asdf", "adsf") Nil)

zipListApplicativeExercise :: IO ()
zipListApplicativeExercise = do
  sectionLabel "ZipList Applicative Exercise"
  putStrLn "testing take'"
  print (toList [1 .. 3] == (toList . take 3 $ [1 .. 5]))
  quickBatch $ applicative (ZipList' (Cons ("as", "asdf", "adsf") Nil))

exampleSuccess :: Validation String (String, Product Int, Sum Int)
exampleSuccess = Success' ("as", Product 3, Sum 5)

validationApplicativeExercise :: IO ()
validationApplicativeExercise = do
  sectionLabel "Validation Applicative Exercise"
  quickBatch $ applicative exampleSuccess

-- Chapter Exercises
-- Specialized versions
-- 1.
type SpecializedList = [Int]

pureSpecializedList :: Int -> SpecializedList
pureSpecializedList = (: [])

applySpecializedList :: [Int -> b] -> SpecializedList -> [b]
applySpecializedList = (<*>)

-- 2.
type SpecializedIO = IO String

pureSpecializedIO :: String -> SpecializedIO
pureSpecializedIO = return

applySpecializedIO :: IO (String -> b) -> SpecializedIO -> IO b
applySpecializedIO = (<*>)

-- 3.
type SpecializedPair = (,) String (Sum Int)

pureSpecializedPair :: Sum Int -> (String, Sum Int)
pureSpecializedPair x = (mempty, x)

applySpecializedPair :: (String, Sum Int -> b) -> SpecializedPair -> (String, b)
applySpecializedPair = (<*>)

-- 4.
type SpecializedFunction = (->) Char

type SpecializedFunction' = Char -> String

-- a function Char -> String
pureSpecializedFunction :: SpecializedFunction'
pureSpecializedFunction = (: [])

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- So a function that goes from Char to b is the output
applySpecializedFunction :: (String -> b) -> SpecializedFunction' -> (Char -> b)
applySpecializedFunction f g = f . g

-- Make instances
-- 1.
data Pair a =
  Pair a a
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Pair a) where
  (<>) (Pair x1 y1) (Pair x2 y2) = Pair (x1 <> x2) (y1 <> y2)

instance (Monoid a) => Monoid (Pair a) where
  mempty = Pair mempty mempty
  mappend = (<>)

instance Functor Pair where
  fmap f (Pair x1 x2) = Pair (f x1) (f x2)

instance Applicative Pair where
  (<*>) (Pair f g) (Pair x1 x2) = Pair (f x1) (g x2)
  pure x = Pair x x

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x1 <- arbitrary
    x2 <- arbitrary
    return (Pair x1 x2)

examplePair :: Pair (String, Product Int, Sum Int)
examplePair = Pair ("dsdf", Product 5, Sum 2) ("asdf", Product 3, Sum 4)

-- 2.
data Two a b =
  Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two x1 y1) (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Monoid a) => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two x1 f) (Two x2 y) = Two (x1 <> x2) (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x1 <- arbitrary
    x2 <- arbitrary
    return (Two x1 x2)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

exampleTwo :: Two (String, Product Int, [String]) (String, String, Sum Int)
exampleTwo = Two ("", Product 0, [""]) ("", "", Sum 0)

-- 3.
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (<>) (Three x1 y1 z1) (Three x2 y2 z2) =
    Three (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend = (<>)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three x1 y1 f) (Three x2 y2 z) = Three (x1 <> x2) (y1 <> y2) (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

exampleThree ::
     Three (String, Product Int, [String]) (String, String, Sum Int) ( Sum Int
                                                                     , Sum Int
                                                                     , Sum Int)
exampleThree = Three ("", Product 0, [""]) ("", "", Sum 0) (Sum 1, Sum 2, Sum 3)

-- 4.
data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Three' a b) where
  (<>) (Three' x1 y1a y1b) (Three' x2 y2a y2b) =
    Three' (x1 <> x2) (y1a <> y2a) (y1b <> y2b)

instance (Monoid a, Monoid b) => Monoid (Three' a b) where
  mempty = Three' mempty mempty mempty
  mappend = (<>)

instance Functor (Three' a) where
  fmap f (Three' x y1 y2) = Three' x (f y1) (f y2)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' x1 f g) (Three' x2 y1 y2) = Three' (x1 <> x2) (f y1) (g y2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y1 <- arbitrary
    y2 <- arbitrary
    return (Three' x y1 y2)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

exampleThree' ::
     Three' (String, Product Int, [String]) (String, String, Sum Int)
exampleThree' = Three' ("", Product 0, [""]) ("", "", Sum 0) ("", "", Sum 0)

-- 5.
data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (<>) (Four w1 x1 y1 z1) (Four w2 x2 y2 z2) =
    Four (w1 <> w2) (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty
  mappend = (<>)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four w1 x1 y1 f) (Four w2 x2 y2 z) =
    Four (w1 <> w2) (x1 <> x2) (y1 <> y2) (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four w x y z)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

exampleFour ::
     Four (String, Product Int, [String]) (String, String, Sum Int) ( Sum Int
                                                                    , Sum Int
                                                                    , Sum Int) ( String
                                                                               , String
                                                                               , String)
exampleFour =
  Four ("", Product 0, [""]) ("", "", Sum 0) (Sum 1, Sum 2, Sum 3) ("", "", "")

-- 6.
data Four' a b =
  Four' a a a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Four' a b) where
  (<>) (Four' w1 x1 y1 z1) (Four' w2 x2 y2 z2) =
    Four' (w1 <> w2) (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Monoid a, Monoid b) => Monoid (Four' a b) where
  mempty = Four' mempty mempty mempty mempty
  mappend = (<>)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' w1 x1 y1 f) (Four' w2 x2 y2 z) =
    Four' (w1 <> w2) (x1 <> x2) (y1 <> y2) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four' w x y z)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

exampleFour' :: Four' (Sum Int, Sum Int, Sum Int) (String, String, String)
exampleFour' =
  Four'
    (Sum 1, Sum 2, Sum 3)
    (Sum 4, Sum 5, Sum 6)
    (Sum 7, Sum 8, Sum 9)
    ("", "", "")

chapterApplicativeInstancesExercise :: IO ()
chapterApplicativeInstancesExercise = do
  sectionLabel "Chapter Applicative Instances Exercise"
  quickBatch $ applicative examplePair
  quickBatch $ applicative exampleTwo
  quickBatch $ applicative exampleThree
  quickBatch $ applicative exampleThree'
  quickBatch $ applicative exampleFour
  quickBatch $ applicative exampleFour'

-- Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (\a b c -> (a, b, c))

combosApplied = combos stops vowels stops

main :: IO ()
main = do
  listApplicativeExercise
  zipListApplicativeExercise
  validationApplicativeExercise
  chapterApplicativeInstancesExercise
  sectionLabel "Chapter Combinations Exercise"
  print combos
