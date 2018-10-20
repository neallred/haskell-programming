-- Monoid laws
--------------

-- left identity
-- mappend mempty x = x

-- right identity
-- mappend x mempty = x

-- associativity
-- mappend x (mappend y z) =
--   mappend (mappend x y) z

-- mconcat = foldr mappend mempty

data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a
  => Monoid (Optional a) where
mempty = Nada
mappend x Nada = x
mappend Nada (Only x) = Only x
mappend (Only x) (Only y) = Only (Prelude.mappend x y)
