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
