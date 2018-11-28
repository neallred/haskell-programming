factorial :: a -> Int -> Int
factorial _ 0 = 1
factorial _ n = n * (factorial 'B' (n - 1) )

factoscan = scanl factorial 1 [1..]


