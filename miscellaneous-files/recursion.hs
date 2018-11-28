-- applyTimes :: (Eq a, Num a) =>
--               a -> (b -> b) -> b -> b
-- applyTimes 0 f b =
--   b
-- applyTimes n f b =
--   f . applyTimes (n-1) f $ b

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n


applyTimes n f b = f (applyTimes (n-1) f b)

-- applyTimes 5 (+1) 5
-- (+1) (applyTimes (5-1) (+1) 5)
-- (+1) ((+1) (applyTimes (4-1) (+1) 5))
-- (+1) ((+1) ((+1) (applyTimes (3-1) (+1) 5)))
-- (+1) ((+1) ((+1) ((+1) (applyTimes (2-1) (+1) 5))))
-- (+1) ((+1) ((+1) ((+1) ((+1) (applyTimes (1-1) (+1) 5)))))
trumm = (+1) ((+1) ((+1) ((+1) ((+1) (5)))))

f :: Bool -> Maybe Int
f False = Just 0
f _ = Nothing

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = if n > 2 then fib (n - 1) + fib (n - 2) else 0

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy ::  String -> String -> String
flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"

-- appedCatty (frappe "blue")
-- 
-- "woops mrow blue mrow haha"

--cattyConny (frappe "pink")
--           (cattyConny "green" (appedCatty "blue"))
--
--cattyConny (frappe "pink")
--           (cattyConny "green" ("woops mrow blue"))
--
--cattyConny (frappe "pink")
--           ("green mrow woops mrow blue")
--
--cattyConny ("haha mrow pink")
--           ("green mrow woops mrow blue")
--
--"pink mrow haha mrow green mrow woops mrow blue"

-- cattyConny (flippy "Pugs" "are") "awesome"
-- 
-- cattyConny ("are mrow Pugs") "awesome"
-- 
-- "are mrow Pugs mrow awesome"

-- dividedBy :: Integral a => a -> a -> (a, a)
-- dividedBy dividend divisor = go dividend divisor 0
--   where go n d count
--           | n < d = (count, n)
--           | otherwise =
--               go (n - d) d (count + 1)

-- dividedBy 15 2
--   = go 15 2 0
--   = go 13 2 1
--   = go 11 2 2
--   = go 9 2 3
--   = go 7 2 4
--   = go 5 2 5
--   = go 3 2 6
--   = go 1 2 7
--   = (7, 1)

sumNum :: (Eq a, Ord a, Num a) => a -> a
sumNum num = go num 0
  where go num runningSum
          | num < 1 = 0
          | otherwise =
              go (num - 1) runningSum + num

intMult :: Integral a => a -> a -> a
intMult x y = go x y 0
  where go x y count
          | y == 0 = count
          | otherwise =
              go x (y - 1) (count + x)

divIsNegative :: Integral a => a -> a -> Bool
divIsNegative x y
  | x < 0 && y >= 0 = True
  | x >= 0 && y < 0 = True
  | otherwise = False

dividedBy :: Integral a => a -> a -> Maybe (a, a)
dividedBy dividend divisor
  | divisor == 0 = Nothing
  | otherwise =
      go (abs dividend) (abs divisor) 0
        where go n d count
                | n < d = Just ((if (divIsNegative dividend divisor) then (negate count) else count), n)
                | otherwise =
                    go (n - d) d (count + 1)

mc91 :: (Integral a) => a -> a
mc91 num
  | num > 100 = num - 10
  | otherwise = mc91 . mc91 $ (num + 11)


