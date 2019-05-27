{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Data.List.NonEmpty
import Data.Monoid (Monoid, Sum, mappend, mempty)

import Data.Semigroup (Semigroup, (<>))
import Test.QuickCheck
import Test.QuickCheck.Function (Fun, applyFun)

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
  monoidExercises

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

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity a) (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

-- 3.
data Two a b =
  Two a b
  deriving (Eq, Show)

pairToTwo :: (a, b) -> Two a b
pairToTwo (x, y) = Two x y

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two x1 y1) (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  x <- arbitrary
  Two x <$> arbitrary

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
  Three x y <$> arbitrary

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
  Four w x y <$> arbitrary

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
  arbitrary = BoolConj <$> arbitrary

-- 7.
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj True) _ = BoolDisj True
  (<>) _ (BoolDisj True) = BoolDisj True
  (<>) _ _ = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

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
    { unCombine :: a -> b
    }

instance (Semigroup b) => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine (f <> g)

combineAssoc ::
     (Eq b, Semigroup s)
  => (Fun a b -> s)
  -> (s -> a -> b)
  -> a
  -> Fun a b
  -> Fun a b
  -> Fun a b
  -> Bool
combineAssoc wrap eval point f g h =
  eval (s1 <> (s2 <> s3)) point == eval ((s1 <> s2) <> s3) point
  where
    s1 = wrap f
    s2 = wrap g
    s3 = wrap h

-- 10.
newtype Comp a =
  Comp
    { unComp :: a -> a
    }

instance Semigroup (Comp a) where
  (<>) (Comp f) (Comp g) = Comp (f . g)

compAssoc ::
     (Eq a, Semigroup s)
  => (Fun a a -> s)
  -> (s -> a -> a)
  -> a
  -> Fun a a
  -> Fun a a
  -> Fun a a
  -> Bool
compAssoc wrap eval x f g h =
  eval ((f' <> g') <> h') x == eval (f' <> (g' <> h')) x
  where
    f' = wrap f
    g' = wrap g
    h' = wrap h

-- 11.
data Validation a b
  = Failure' a
  | Success' b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary =
    frequency [(1, fmap Failure' arbitrary), (1, fmap Success' arbitrary)]

-- frequency, not this pattern.
instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Success' x) _ = Success' x
  (<>) (Failure' x) (Success' y) = Success' y
  (<>) (Failure' x) (Failure' y) = Failure' (x <> y)

validationExercise = do
  let failure :: String -> Validation String Int
      failure = Failure'
      success :: Int -> Validation String Int
      success = Success'
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

type SumGen = Fun (Sum Int) (Sum Int)

semigroupExercises :: IO ()
semigroupExercises = do
  putStrLn "Checking Trival semigroup property:"
  quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  putStrLn "Checking Identity a semigroup property:"
  quickCheck
    (semigroupAssoc :: Identity String -> Identity String -> Identity String -> Bool)
  putStrLn "Checking Two a b semigroup property:"
  quickCheck
    (semigroupAssoc :: Two String (Sum Int) -> Two String (Sum Int) -> Two String (Sum Int) -> Bool)
  putStrLn "Checking Three a b c semigroup property:"
  quickCheck
    (semigroupAssoc :: Three String Int [[String]] -> Three String Int [[String]] -> Three String Int [[String]] -> Bool)
  putStrLn "Checking BoolConj semigroup property:"
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  putStrLn "Checking BoolDisj semigroup property:"
  quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  putStrLn "Checking Or a b semigroup property:"
  quickCheck (semigroupAssoc :: OrIntsTest)
  putStrLn "Checking Combine a b semigroup property:"
  quickCheck
    (combineAssoc (Combine . applyFun) unCombine :: Int -> Fun Int (Sum Int) -> Fun Int (Sum Int) -> Fun Int (Sum Int) -> Bool)
  putStrLn "Checking Comp a a semigroup property:"
  quickCheck
    (compAssoc (Comp . applyFun) unComp :: Sum Int -> SumGen -> SumGen -> SumGen -> Bool)
  putStrLn "Checking Validation a b semigroup property:"
  quickCheck
    (semigroupAssoc :: Validation String Int -> Validation String Int -> Validation String Int -> Bool)

-- Monoid exercises
-- 1.
instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2.
instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

type IdentityAssoc
   = Identity String -> Identity String -> Identity String -> Bool

-- 3.
instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

type TwoAssoc
   = Two (Sum Int) String -> Two (Sum Int) String -> Two (Sum Int) String -> Bool

-- 4.
instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

type Bool4 = BoolConj -> BoolConj -> BoolConj -> Bool

-- 5.
instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 6.
instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine $ const mempty
  mappend = (<>)

combineLeftIdentity ::
     (Eq b, Monoid m) => (Fun a b -> m) -> (m -> a -> b) -> a -> Fun a b -> Bool
combineLeftIdentity wrap eval point f = eval (mempty <> m) point == eval m point
  where
    m = wrap f

combineRightIdentity ::
     (Eq b, Monoid m) => (Fun a b -> m) -> (m -> a -> b) -> a -> Fun a b -> Bool
combineRightIdentity wrap eval point f =
  eval (m <> mempty) point == eval m point
  where
    m = wrap f

combineMonoidAssoc ::
     (Eq b, Semigroup m, Monoid m)
  => (Fun a b -> m)
  -> (m -> a -> b)
  -> a
  -> Fun a b
  -> Fun a b
  -> Fun a b
  -> Bool
combineMonoidAssoc wrap eval point f g h =
  eval (m1 <> (m2 <> m3)) point == eval ((m1 <> m2) <> m3) point
  where
    m1 = wrap f
    m2 = wrap g
    m3 = wrap h

-- 7.
instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

compAssocMonoid ::
     (Eq a, Semigroup m, Monoid m)
  => (Fun a a -> m)
  -> (m -> a -> a)
  -> a
  -> Fun a a
  -> Fun a a
  -> Fun a a
  -> Bool
compAssocMonoid wrap eval x f g h =
  eval ((f' <> g') <> h') x == eval (f' <> (g' <> h')) x
  where
    f' = wrap f
    g' = wrap g
    h' = wrap h

-- 8.
newtype Mem s a =
  Mem
    { runMem :: s -> (a, s)
    }

instance (Semigroup a, Monoid a) => Semigroup (Mem s a) where
  (<>) x y =
    let partialX = runMem x
        partialY = runMem y
        f s =
          let (a1, s2) = partialX s
              (a2, s3) = partialY s2
           in (a1 <> a2, s3)
     in Mem f

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (mempty, )
  -- mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)

fMem = Mem $ \s -> ("hi", s + 1)

(Mem hi) = Mem $ \s -> ("hi", s + 1)

fMem2 = Mem $ \s -> ("forty-two", s * 31)

exampleStateness = do
  let rmzero = runMem mempty 0
      rmleft = runMem (fMem <> mempty) 0
      rmright = runMem (mempty <> fMem) 0
  print rmleft -- ("hi", 1)
  print rmright -- ("hi", 1)
  print (rmzero :: (String, Int)) -- ("hi", 0)
  print $ rmleft == runMem fMem 0 -- True
  print $ rmright == runMem fMem 0 -- True

monoidExercises :: IO ()
monoidExercises = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity
  putStrLn "Checking Trivial monoid properties"
  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mri :: Trivial -> Bool)
  putStrLn "Checking Identity a monoid properties"
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  putStrLn "Checking BoolConj Bool monoid properties"
  quickCheck (semigroupAssoc :: Bool4)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  putStrLn "Checking Two a b monoid properties"
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two (Sum Int) String -> Bool)
  quickCheck (monoidRightIdentity :: Two (Sum Int) String -> Bool)
  putStrLn "Checking BoolDisj monoid properties"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  putStrLn "Checking Combine a b monoid properties"
  quickCheck
    (combineMonoidAssoc (Combine . applyFun) unCombine :: Int -> Fun Int (Sum Int) -> Fun Int (Sum Int) -> Fun Int (Sum Int) -> Bool)
  quickCheck
    (combineLeftIdentity (Combine . applyFun) unCombine :: Int -> Fun Int (Sum Int) -> Bool)
  quickCheck
    (combineRightIdentity (Combine . applyFun) unCombine :: Int -> Fun Int (Sum Int) -> Bool)
  putStrLn "Checking Comp a monoid properties"
  quickCheck
    (compAssocMonoid (Comp . applyFun) unComp :: Sum Int -> Fun (Sum Int) (Sum Int) -> Fun (Sum Int) (Sum Int) -> Fun (Sum Int) (Sum Int) -> Bool)
  quickCheck
    (combineLeftIdentity (Comp . applyFun) unComp :: Sum Int -> Fun (Sum Int) (Sum Int) -> Bool)
  quickCheck
    (combineRightIdentity (Comp . applyFun) unComp :: Sum Int -> Fun (Sum Int) (Sum Int) -> Bool)
  putStrLn "Checking Mem s a monoid properties"
--  quickCheck (monoidAssoc :: Mem Int String -> Mem Int String -> Mem Int String -> Bool)
--  Does not have Eq instance. Does it make sense for it to? I don't think so.
--  Functions do not have an Eq instance, and you construct a Mem by passing in a function.
