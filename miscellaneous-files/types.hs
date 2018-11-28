{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- simple example
example = 1

-- a = 12 + b
b = 10000 * 30

functionH :: [a] -> a
functionH (x:_) = x 

functionC :: (Ord a) => a -> a -> Bool
functionC x y =
  x > y

functionS :: (a,b) -> b
functionS (x, y) = y


myFunc :: (x -> y)
       -> (y -> z)
       -> c
       -> (a, x)
       -> (a, z)
myFunc xToY yToZ _ (a, x) =
  (a, yToZ ( xToY x)) 

i :: a -> b -> b
i _ b  = b

r :: [a] -> [a]
r [] = []
r x = reverse x

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC (aToB a)

a :: (a -> c) -> a -> a
a _ a = a

a' :: (a->b) -> a -> b
a' aToB a = aToB a

convList :: a -> [a]
convList thing =
  [thing, thing, thing]

-- Doesn't work
-- blahFunc :: b -> String
-- blahFunc "bob" = "dole"
-- blahFunc _ = "doesn't need this"
