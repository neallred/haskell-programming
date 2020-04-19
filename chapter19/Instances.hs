module Instances where

data Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x 
  foldMap f (Identity x) = f x

data Optional a = Nada | Yep a

instance Foldable Optional where
  foldr _ z (Nada) = z
  foldr f z (Yep x) = f x z 

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

-- library functions

-- 1.
sum' :: (Foldable t, Num a) => t a -> a
sum' xs = foldr (+) 0 xs

-- 2.
product' :: (Foldable t, Num a) => t a -> a
product' xs = foldr (*) 0 xs

-- 3.
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a bs = foldr (\x acc -> acc || x == a) False bs
