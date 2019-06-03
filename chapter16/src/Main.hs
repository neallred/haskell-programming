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
  deriving (Eq, Show)

-- I think this is really a "b", and that "b" while a is not is now witnessed??
instance Functor (Flip K' a) where
  fmap f (Flip (K' x)) = Flip (K' (f x))

instance (Arbitrary b) => Arbitrary (Flip K' a b) where
  arbitrary = do
    x <- arbitrary
    return (Flip (K' x))

-- 4.
data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = fmap GoatyConst arbitrary

-- 5.
data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut functor) = LiftItOut (fmap f functor)

-- genLiftItOut :: (Functor f, Arbitrary a) => Gen (LiftItOut f a)
-- genLiftItOut = LiftItOut ((arbitrary :: Gen (Maybe Int)) arbitrary)
--frequency [(1, return Nil), (20, Cons <$> arbitrary <*> myList)]
-- instance (Functor f, Arbitrary a) => Arbitrary (LiftItOut f a) where
--   arbitrary = do
--     let trumm f a = f a
--     x <- arbitrary
--     y <- arbitrary
--     let plz = x y
--     return (LiftItOut plz)
-- instance (Functor f, Arbitrary a) => Arbitrary (LiftItOut f a) where
--   arbitrary = do
--     x <- arbitrary
--     y <- arbitrary
--     let duzntwrk = fmap y ((fmap x) LiftItOut)
--     duzntwrk
--    return duzntwrk
-- this type checks but does not do anything useful, seems to call itself infinitely
--  arbitrary = do
--    x <- arbitrary
--    return x
--
--  Close, unless its totaly just depending on the call to itself doing all the work.
--  gives               LiftItOut Gen (LiftItOut f a)
--  but needs to be     Gen (LiftItOut f a)
--  arbitrary = do
--    x <- arbitrary
--    ((fmap x) (LiftItOut (arbitrary)))
--   arbitrary = do
--     x <- arbitrary
--     return ((fmap x) (LiftItOut (arbitrary)))
-- arbitrary = return ((fmap arbitrary) (LiftItOut (arbitrary)))
--  arbitrary = return (LiftItOut ((fmap arbitrary) f))
-- instance Functor f => Functor (Wrap f) where
--   fmap f (Wrap fa) = Wrap (fmap f fa)
-- instance (Functor f, Monoid a, CoArbitrary a, Arbitrary a, Arbitrary (f a)) =>
--          Arbitrary (LiftItOut f a) where
--   arbitrary = do
--     x <- arbitrary
--     return (LiftItOut (fmap mempty x))
--
-- 6.
data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap g (DaWrappa f1 f2) = DaWrappa (fmap g f1) (fmap g f2)

-- 7.
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x secondFunctor) =
    IgnoringSomething x (fmap f secondFunctor)

-- instance Arbitrary (IgnoreOne f g a b) where
--   arbitrary = IgnoringSomething
-- 8.
data Notorious g o a t =
  Notorious (g o) (g a) (g t)
  deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap g (Notorious f1 f2 f3) = Notorious f1 f2 (fmap g f3)

-- 9.
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Thanks much https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
myList :: Arbitrary a => Gen (List a)
myList = frequency [(1, return Nil), (20, Cons <$> arbitrary <*> myList)]

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = myList

-- 10.
data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

genGoatLord :: (Arbitrary a) => Gen (GoatLord a)
genGoatLord =
  frequency
    [ (1, return NoGoat)
    , (3, fmap OneGoat arbitrary)
    , (2, MoreGoats <$> genGoatLord <*> genGoatLord <*> genGoatLord)
    ]

instance (Arbitrary a) => Arbitrary (GoatLord a) where
  arbitrary = genGoatLord

-- 11.
data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x y) = Print x (f y)
  fmap f (Read g) = Read (f . g)

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
  -- 3.
  labelId "Flip K' [Int] String"
  quickCheck (functorIdentity :: Flip K' [Int] String -> Bool)
  labelComp "Flip K' [Int] String"
  quickCheck
    (functorCompose (fmap toUpper) (++ "BOB") :: Flip K' [Int] String -> Bool)
  -- 4.
  labelId "EvilGoateeConst Bool String"
  quickCheck (functorIdentity :: EvilGoateeConst Bool String -> Bool)
  labelComp "EvilGoateeConst Bool String"
  quickCheck
    (functorCompose (fmap toUpper) (++ "BOB") :: EvilGoateeConst Bool String -> Bool)
  -- 9.
  labelId "List Int"
  quickCheck (functorIdentity :: List Int -> Bool)
  labelComp "List Int"
  quickCheck (functorCompose (* 9001) (* 42) :: List Int -> Bool)
  -- 10.
  labelId "GoatLord Int"
  quickCheck (functorIdentity :: GoatLord Int -> Bool)
  labelComp "GoatLord Int"
  quickCheck (functorCompose (* 9001) (* 42) :: GoatLord Int -> Bool)

--
-- 5.
--  labelId "LiftItOut Maybe String"
--  quickCheck (functorIdentity :: LiftItOut Maybe String -> Bool)
--  labelComp "LiftItOut Maybe String"
--  quickCheck
--    (functorCompose (fmap toUpper) (++ "BOB") :: LiftItOut Maybe String -> Bool)
-- Standing questions: What is needed to generate Arbitrary instances for arbitrary functors?
-- For arbitrary functions? Seems like you'd use CoArbitrary
main :: IO ()
main = do
  instancesOfFuncExercises
  possiblyExercise
  sumExercise
  functorInstances
