{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter16 where

class Sumthin a where
  s :: a -> a

-- class Else where
--   e :: b -> f (g a b c)
-- class Biffy where
--   slayer :: e a b -> (a -> c) -> (b -> d) -> e c d
-- class Impish v where
--   impossibleKind :: v -> v a
-- 
-- class AlsoImp v where
--   nope :: v a -> v
-- Exercises: Be Kind
-- 1. Given f :: a -> a, kind of a is *
-- 2. Given f :: a -> b a -> T (b a), kind of b is * -> *, kind of T is * -> *
-- 3. Given f :: c a b -> c b a, kind of c is * -> * -> *
-- *Chapter16> :t ($)
-- ($) :: (a -> b) -> a -> b
-- *Chapter16> :t (<$>)
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
data FixMePls a
  = FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

data WhoCares a
  = ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)

data CountingBad a =
  Heisenberg Int a
  deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n + 1) (f a)

data CountingGood a =
  Heisenberger Int a
  deriving (Eq, Show)

instance Functor CountingGood where
  fmap f (Heisenberger n a) = Heisenberger n (f a)

-- Exercise: Wait, how does that even typecheck?
-- (.) :: (b -> c) -> (a -> b) -> a c
-- fmap :: Functor f => (m -> n) -> f m -> f n
-- fmap :: Functor g => (x -> y) -> g x -> g y
composeStuff00 :: (b -> c) -> (a -> b) -> a -> c
composeStuff00 = undefined

composeStuff01 ::
     (((m -> n) -> f m) -> f n) -> (((x -> y) -> g x) -> g y) -> a -> c
composeStuff01 = undefined

composeStuff02 ::
     (((m -> n) -> f m) -> f n)
  -> (((x -> y) -> g x) -> g y)
  -> ((x -> y) -> g x)
  -> c
composeStuff02 = undefined

composeStuff03 ::
     (((m -> n) -> f m) -> f n)
  -> (((x -> y) -> g x) -> g y)
  -> ((x -> y) -> g x)
  -> f n
composeStuff03 = undefined

-- then if you apply the concrete value, which is a function returning an applied value, you get
-- composeStuff03' ::
--      (((m -> n) -> f m) -> f n)
--   -> (((x -> y) -> g x) -> g y) ((x -> y) -> g x)
--   -> f n
-- composeStuff03' = undefined
--
composeStuff04 :: (((m -> n) -> f m) -> f n) -> g y -> f n
composeStuff04 = undefined

-- then if consider that the definition of compose is that g y *is* b and ((m -> n) -> f m) *is* b, you get
composeStuff05 :: (g y -> f n) -> g y -> f n
composeStuff05 = undefined

-- were you to apply the value again, you'd get
-- composeStuff05' :: (g y -> f n) g y -> f n
-- composeStuff05' = undefined
composeStuff06 :: f n
composeStuff06 = undefined

ha = Just ["Ha", "Ha"]

lmls = [ha, Nothing, Just []]

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [[Maybe Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f2) => f1 (f2 a) -> f1 (f2 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted ::
     (Functor f1, Functor f2, Functor f3) => f1 (f2 (f3 a)) -> f1 (f2 (f3 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

exampleLifting :: IO ()
exampleLifting = do
  putStr "replaceWithP' lms: "
  print (replaceWithP' lms)
  putStr "replaceWithP' lms: "
  print (replaceWithP' lms)
  putStr "twiceLifted lms    "
  print (twiceLifted lms)
  putStr "twiceLifted' lms   "
  print (twiceLifted' lms)
  putStr "thriceLifted lms   "
  print (thriceLifted lms)
  putStr "thriceLifted' lms  "
  print (thriceLifted' lms)

-- Exercises: Heavy Lifting
-- 1.
heavyLiftingOne = fmap (+ 1) $ read "[1]" :: [Int]

heavyLiftingTwo = (fmap . fmap $ (++ "lol")) (Just ["Hi,", "Hello"])

heavyLiftingThree = fmap (* 2) (\x -> x - 2)

heavyLiftingThree' = (* 2) . (\x -> x - 2)

-- Is there something funny with this one?
-- if you give it `1` in GHCi, you get a never ending list of ones printing out
heavyLiftingFour = fmap ((return '1' ++) . show) (\x -> [x,1 .. 3])

heavyLiftingFive :: IO Integer
heavyLiftingFive =
  let ioi = readIO "1" :: IO Integer
      changed = fmap (read . ("123" ++)) (fmap show ioi)
   in fmap (* 3) changed

-- Exercises: Instances of Func
-- See chapter16/src/Main.hs
incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust _ = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust _ = Nothing

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+ 1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe = fmap show

-- Exercise :: Possibly
-- See chapter16/src/Main.hs
-- Exercise: Either Short Exercise
-- See chapter16/src/Main.hs
newtype Constant a b =
  Constant
    { getConstant :: a
    }
  deriving (Eq, Show)

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v

data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

--
-- getLine :: IO String
-- read :: Read a => String -> a
getInt :: IO Int
getInt = fmap read getLine

-- nat :: (f -> g) -> f a -> g a
type Nat f g = forall a. f a -> g a

data Tuple a b =
  Tuple a b
  deriving (Eq, Show)

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

-- Chpater exercises
-- determine if a valid Functor can be written for the datatype
-- 1.
data Bool'
  = Falselio
  | Truelio

-- No, it is kind *
-- 2.
data BoolAndSomethingElse a
  = False' a
  | True' a

-- Yep! the a makes it * -> *
-- It would be like having data Something a = First a | Second a
-- 3.
data BoolAndMaybeSomethingElse a
  = Falsish
  | Truish a

-- Yep. Kind is * -> *.
-- It is Maybe, recycled
-- 4.
newtype Mu f =
  InF
    { outF :: f (Mu f)
    } --
-- Yes. It is kind (* -> *) -> *
-- You can apply any type that itself is * -> *. Like Maybe, or (Either a)
