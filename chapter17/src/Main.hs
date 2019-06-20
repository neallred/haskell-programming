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

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = do
  sectionLabel "List Applicative Exercise"
  let bob = Cons 3 Nil <> Cons 4 Nil :: List (Sum Int)
  print bob
  quickBatch $ applicative (Cons (2 :: Sum Int, "asdf", "adsf") Nil)

ziplistApplicativeExercise :: IO ()
ziplistApplicativeExercise = do
  sectionLabel "List Applicative Exercise"
  sectionLabel "List Applicative Exercise"
