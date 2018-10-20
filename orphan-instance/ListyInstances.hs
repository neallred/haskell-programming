module ListyInstances where

import Data.Monoid
import Listy

newtype Listy a =
  Listy [a]
  deriving (Eq, Show)

instance Monoid (Listy a) where
  mempty = Listy []
  mappend (Listy l) (Listy l') =
    Listy $ mappend l l'
