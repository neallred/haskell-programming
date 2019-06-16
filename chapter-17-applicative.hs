module Chapter17 where

import Control.Applicative
import Data.List (elemIndex)
import Data.Semigroup

-- Applicatives are monoidal functors
main :: IO ()
main = putStrLn "Hello Applicative"

-- fmap
-- ($)   ::                    (a -> b) ->   a ->   b
-- (<$>) :: Functor f =>       (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- Control.Applicative supplies
-- note that its CLOSE to, but distinct from, fmap
-- the structure is an applicative rather than a functor
-- liftA :: Applicative f => (a -> b) -> f a -> f b
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
--
-- :t pure :: Applicative a => a -> f a
-- So pure isn't about wrapping a function in an applicative structure
-- It's about wrapping a value into an applicative structure
-- And fuctions happen to be values
-- And its not about creating arbitrary structure
-- so pure 1 :: ([a], Int)
-- is going to return ([], 1)
-- Because it is about preserving structure, because it is a functor
-- pure is mempty for Applicative??
-- Are "applicative functor" and "monoidal functor" the same thing?
--
-- Woah:
-- mappend :: f           f      f
-- $       :: (a -> b)    a      b
-- (<*>)   :: (a -> b) -> f a -> f b
-- We have a Monoid for our structure and function application for our values!
--
-- Uses I can think of:
-- * If you fail to create a function, as in if you Maybe return a function
--
-- Monoid and Applicative instances are not required or guaranteed to have the same monoid of structure, and the functorial part may change the way it behaves.
--
-- Seems like a rule of thumb is if you could mappend OR apply a function, prefer applying the function.
-- And mappending as a back up where it doesn't make sense??
--
-- (<*>) :: f  (a -> b) -> f  a -> f  b
-- (<*>) :: [] (a -> b) -> [] a -> [] b
-- (<*>) :: [] (a -> b) -> [a ] -> [b]
-- <*> is called "apply"?
--
f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]

g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]

m x = lookup x [(4, 10), (8, 13), (1, 9001)]

-- Exercises: Lookups
-- 1.
lookupOne :: Maybe Integer
lookupOne = (+ 3) <$> lookup 3 (zip [1 .. 3] [4 .. 6])

-- 2.
lookupTwoY :: Maybe Integer
lookupTwoY = lookup 3 $ zip [1 .. 3] [4 .. 6]

lookupTwoZ :: Maybe Integer
lookupTwoZ = lookup 2 $ zip [1 .. 3] [4 .. 6]

lookupTwoTupled :: Maybe (Integer, Integer)
lookupTwoTupled = pure (,) <*> lookupTwoY <*> lookupTwoZ

-- 3.
lookupThreeX :: Maybe Int
lookupThreeX = elemIndex 3 [1 .. 5]

lookupThreeY :: Maybe Int
lookupThreeY = elemIndex 4 [1 .. 5]

lookupThreeMax' :: Int -> Int -> Int
lookupThreeMax' = max

lookupThreeMaxed :: Maybe Int
lookupThreeMaxed = pure lookupThreeMax' <*> lookupThreeX <*> lookupThreeY

-- 4.
lookupFourXs = [1 .. 3]

lookupFourYs = [4 .. 6]

lookupFourX :: Maybe Integer
lookupFourX = lookup 3 $ zip lookupFourXs lookupFourYs

lookupFourY :: Maybe Integer
lookupFourY = lookup 2 $ zip lookupFourXs lookupFourYs

summed :: Maybe Integer
summed = pure sum <*> (pure (,) <*> lookupFourX <*> lookupFourY)

--
-- f ~ Identity
-- Applicative f =>
-- type Id = Identity
-- (<*>) :: f  (a -> b) ->  f a ->  f b
-- (<*>) :: Id (a -> b) -> Id a -> Id b
-- 
-- pure :: a ->  f a
-- pure :: a -> Id a
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity
  -- pure a = Identity a
                         where
  pure = Identity
  -- (<*>) (Identity f) thingThatsAFunctor = fmap f thingThatsAFunctor
  (<*>) (Identity f) = fmap f
  -- failed attempts:
  -- (<*>) f thingThatsAFunctor = pure f <*> pure thingThatsAFunctor
  -- (<*>) f thingThatsAFunctor = pure (f thingThatsAFunctor)
  -- (<*>) f thingThatsAFunctor = (f thingThatsAFunctor)
  -- (<*>) f thingThatsAFunctor = (fmap f thingThatsAFunctor)
  -- (<*>) f fA = fmap f fA
  -- (<*>) f (Identity a) = f <*> (Identity a)

--
newtype Constant a b =
  Constant
    { getConstant :: a
    }
  deriving (Eq, Show)

type C = Constant

-- (<*>) ::   f (a -> b) ->   f a ->   f b
-- (<*>) :: C e (a -> b) -> C e a -> C e b
--
-- pure :: a ->   f a
-- pure :: a -> C e a
instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance (Semigroup a) => Semigroup (Constant a b) where
  (<>) (Constant x1) (Constant x2) = Constant (x1 <> x2)

instance (Semigroup a, Monoid a) => Monoid (Constant a b) where
  mempty = mempty
  mappend = (<>)

instance (Semigroup a, Monoid a) => Applicative (Constant a) where
  pure x = Constant mempty
  (<*>) (Constant x0) (Constant x) = Constant (x0 <> x)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
    then Nothing
    else Just s

newtype Name =
  Name String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = Address <$> validateLength 100 a

data Person =
  Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  case mkName n of
    Nothing -> Nothing
    Just n' ->
      case mkAddress a of
        Nothing -> Nothing
        Just a' -> Just $ Person n' a'

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = pure Person <*> mkName n <*> mkAddress a

mkPerson'' :: String -> String -> Maybe Person
mkPerson'' n a = Person <$> mkName n <*> mkAddress a

data Cow =
  Cow
    { name :: String
    , age :: Int
    , weight :: Int
    }
  deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative x
  | x >= 0 = Just x
  | otherwise = Nothing

makeCow :: String -> Int -> Int -> Maybe Cow
makeCow name age weight =
  Cow <$> noEmpty name <*> noNegative age <*> noNegative weight

makeCow' :: String -> Int -> Int -> Maybe Cow
makeCow' name age weight =
  liftA3 Cow (noEmpty name) (noNegative age) (noNegative weight)

maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeApply = (<*>)

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap = fmap

-- maybeMapBad :: (a -> b) -> Maybe a -> f b
-- maybeMapBad = fmap
-- rigid type variable is a funny name to me,
-- because it means it can be any type at all, which doesn't sound rigid
-- but it makes sense because when you get this error, you are forcing it to be a certain type or
-- at least follow certain constraints, like Num a, or Maybe a
--
-- Exercise: Fixer Upper
-- 1.
fixerUpperOne = const <$> Just "Hello" <*> pure "World"

-- 2.
fixerUpperTwo =
  fmap (,,,) (Just 90) <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3] -- Applicative Laws
-- Law 1: Identity
-- pure id <*> v = v
-- Law 2: Composition
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- Law 3: Homomorphism
-- pure f <*> pure x = pure (f x)
-- Law 4: Interchange
-- u <*> pure y = pure ($ y) <*> u
--
