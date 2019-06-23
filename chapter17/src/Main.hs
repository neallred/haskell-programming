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
type SpecializedList = [Int]

pureSpecializedList :: Int -> SpecializedList
pureSpecializedList = (: [])

applySpecializedList :: [Int -> b] -> SpecializedList -> [b]
applySpecializedList = (<*>)

main :: IO ()
main = do
  listApplicativeExercise
  zipListApplicativeExercise
  validationApplicativeExercise
