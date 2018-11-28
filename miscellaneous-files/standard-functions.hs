myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if f x then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = if x == y then True else myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' _ [] = False
myElem' x y = any (==x) y

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = (f x) ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x1:x2:xs)
  | f x1 x2 == GT = myMaximumBy f (x1:xs)
  | f x1 x2 == LT = myMaximumBy f (x2:xs)
  | otherwise = myMaximumBy f (x1:xs)
myMaximumBy _ (x:[]) = x
myMaximumBy _ [] = undefined

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x1:x2:xs)
  | f x1 x2 == LT = myMinimumBy f (x1:xs)
  | f x1 x2 == GT = myMinimumBy f (x2:xs)
  | otherwise = myMinimumBy f (x1:xs)
myMinimumBy _ (x:[]) = x
myMinimumBy _ [] = undefined

minimum' :: Ord a => [a] -> a
minimum' x = myMinimumBy (compare) x

maximum' :: Ord a => [a] -> a
maximum' x = myMaximumBy (compare) x


