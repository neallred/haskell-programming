-- arith4.hs
module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main = do
  print (roundTrip 4)
  print (id 4)

roundTrip2 :: (Show a, Read a) => a -> a
roundTrip2 = read . show

roundTrip3 :: (Show a, Read b) => a -> b
roundTrip3 = read . show

data SumOfThree a
  = FirstPossible  a
  | SecondPossible a
  | ThirdPossible  a
  deriving (Eq, Show, Ord)

sumToInt :: SumOfThree a -> Integer
sumToInt (FirstPossible  _) = 0
sumToInt (SecondPossible _) = 1
sumToInt (ThirdPossible  _) = 2

