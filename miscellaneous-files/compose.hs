-- f :: (Num a) => a -> a
-- f = negate . sum

f2 :: Int -> [Int] -> Int
f2 = foldr (+)
