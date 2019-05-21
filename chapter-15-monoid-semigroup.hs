module Chapter15 where

import Data.Monoid
import Data.Semigroup
import Test.QuickCheck

-- Semigroup - bijective associative function
-- Monoid - Semigroup with an identity
data Booly a
  = False'
  | True'
  deriving (Eq, Show)

instance Semigroup (Booly a) where
  (<>) False' _ = False'
  (<>) _ False' = False'
  (<>) True' True' = True'

instance Monoid (Booly a) where
  mempty = False'

--  mappend = (<>)
-- Excercise: Optional Monoid
data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada (Only x) = Only x
  (<>) (Only x) Nada = Only x
  (<>) (Only x) (Only y) = Only (x <> y)

instance (Monoid a, Semigroup a) => Monoid (Optional a) where
  mempty = Nada

type Verb = String

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said " <> adv <> " as he jumped into his car " <> noun <>
  " and drove off with his " <>
  adj <>
  " wife."

-- Exercise: madlibbinBetter'
madlibbinBetter :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter e adv noun adj =
  mconcat
    [ e
    , "! he said "
    , adv
    , " as he jumped into his car "
    , noun
    , " and drove off with his "
    , adj
    , " wife."
    ]
-- Chapter Exercises:
-- Semigroup Exercises
-- See chapter15/src/Main.hs
