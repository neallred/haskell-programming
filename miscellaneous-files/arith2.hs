-- arith2.hs
module Arith2 where

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)


main :: IO ()
main = do
  print (0 :: Int)
  print (add 1 0)
  print (addOne 0)
  print (addOnePF 0)
  print ((addOne . addOne) 0)
  print ((addOnePF . addOne) 0)
  print ((addOne . addOnePF) 0)
  print ((addOnePF . addOnePF) 0)
  print (negate (addOne 0))
  print ((negate . addOne) 0)
  print ((addOne . addOne . addOne . negate . addOne) 0)

tensDigit :: Integral a => a -> a
tensDigit x = mod  (fst (x `divMod` 10)) 10
-- tensDigit x = d
--   where xLast = x `div` 10
--         d = xLast `mod` 10

hunsD :: Integral a => a -> a
hunsD x = mod  (fst (x `divMod` 100)) 100

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b =
  if b == True
     then y
     else x

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
  | b == False = x
  | b == True = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a,c) = (f a, c)

