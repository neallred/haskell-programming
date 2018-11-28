trumm = filter ((=='p') . (flip (!!) 0)) [i : j : k : [] | i <- "pbtdkg", j <- "aeiou", k <- "pbtdkg"]

nouns =
  [ "code monkey"
  , "coffee cake"
  , "nap"
  , "bath"
  , "manager"
  , "login page"
  ]
verbs =
  [ "run"
  , "jump"
  , "hide"
  , "see"
  , "take"
  , "build"
  ]

trummd = [i ++ " " ++ j ++ " " ++ k | i <- nouns, j <- verbs, k <- nouns]
--filter ((=='p') . (flip (!!) 0)) 

averageWordLength x =
  (fromIntegral $ sum (map length (words x))) /
  (fromIntegral $ length (words x))

-- 0
myAnd :: [Bool] -> Bool
myAnd = foldr test True
  where test a b = if b then a else b

-- 1
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> b || f a) False

-- 3
myElemFold :: Eq a => a -> [a] -> Bool
myElemFold x = foldr (\y1 y2 -> y2 || check y1) False
  where check = (==) x

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny x = any ((==) x)

-- 4
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> (f x):y) []

-- 6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x xs -> if f x then x:xs else xs) []

-- 7
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x xs -> (f x) ++ xs) []

-- 9
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10
myMaximumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMaximumBy f xs = foldr (\y z -> if (f y z) == GT then y else z) (last xs) xs

-- 11
myMinimumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMinimumBy f xs = foldr (\y z -> if (f y z) == LT then y else z) (last xs) xs
