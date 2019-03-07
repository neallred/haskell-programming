incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + incTimes (times - 1) n

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n - 1) f b)

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+ 1) n

applyTimes' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes' 0 f b = b
applyTimes' n f b = f . applyTimes (n - 1) f $ b

applyTimesExerciseA = applyTimes 5 (+ 1) 5

applyTimesExerciseB = (+ 1) (applyTimes 4 (+ 1) 5)

applyTimesExerciseC = (+ 1) ((+ 1) (applyTimes 3 (+ 1) 5))

applyTimesExerciseD = (+ 1) ((+ 1) ((+ 1) (applyTimes 2 (+ 1) 5)))

applyTimesExerciseE = (+ 1) ((+ 1) ((+ 1) ((+ 1) (applyTimes 1 (+ 1) 5))))

applyTimesExerciseF =
  (+ 1) ((+ 1) ((+ 1) ((+ 1) ((+ 1) (applyTimes 0 (+ 1) 5)))))

applyTimesExerciseG = (+ 1) ((+ 1) ((+ 1) ((+ 1) ((+ 1) (5)))))

applyTimesExerciseH = (+ 1) ((+ 1) ((+ 1) ((+ 1) ((+ 1) 5))))

applyTimesExerciseI = (+ 1) ((+ 1) ((+ 1) ((+ 1) 6)))

applyTimesExerciseJ = (+ 1) ((+ 1) ((+ 1) 7))

applyTimesExerciseK = (+ 1) ((+ 1) 8)

applyTimesExerciseL = (+ 1) 9

applyTimesExerciseM = 10

checkApplyTimesExercise = foldl (\acc curr -> acc && curr == 10) True

applyTimesExerciseCorrect =
  checkApplyTimesExercise
    [ applyTimesExerciseA
    , applyTimesExerciseB
    , applyTimesExerciseC
    , applyTimesExerciseD
    , applyTimesExerciseE
    , applyTimesExerciseF
    , applyTimesExerciseG
    , applyTimesExerciseH
    , applyTimesExerciseI
    , applyTimesExerciseJ
    , applyTimesExerciseK
    , applyTimesExerciseL
    , applyTimesExerciseM
    ]

fibonacci :: Word -> Word
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

divideStuff m n
  | m - n <= n = (1, m - n)
  | otherwise =
    let result = (divideStuff (m - n) n)
     in (1 + fst result, snd result)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

-- Chapter Exercises
--------------------
-- Review of Types
------------------
-- 1. d) [[Bool]]
-- 2. b) [[3 == 3]], [6 > 5], [3 < 4]]
-- 3. d) all of the above
-- 4. b) func "Hello" "World"
--
-- Reviewing Currying
---------------------
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- 1. "woops mrow woohoo!"
-- 2. "1 mrow haha"
-- 3. "woops mrow 2 mrow haha"
-- 4. "woops mrow blue mrow haha"
-- 5. "pink mrow haha mrow green mrow woops mrow blue"
-- 6. "are mrow Pugs mrow awesome"
-- Recursion
------------
-- 1.
--dividedBy 15 2
-- go 15 2 0
-- otherwise = go (15 - 2) 2 (0 + 1)
-- go 13 2 1
-- otherwise = go (13 - 2) 2 (1 + 1)
-- go 11 2 2
-- otherwise = go (11 - 2) 2 (2 + 1)
-- go 9 2 3
-- otherwise = go (9 - 2) 2 (3 + 1)
-- go 7 2 4
-- otherwise = go (7 - 2) 2 (4 + 1)
-- go 5 2 5
-- otherwise = go (5 - 2) 2 (5 + 1)
-- go 3 2 6
-- otherwise = go (3 - 2) 2 (6 + 1)
-- go 1 2 7
-- 1 < 2 = (7, 1)
-- 2.
addThroughN :: (Eq a, Num a) => a -> a
addThroughN x = go x 0
  where
    go n count
      | 1 == n = count + 1
      | otherwise = go (n - 1) (count + n)

-- 3.
recursum :: (Integral a) => a -> a -> a
recursum x y = go x y 0
  where
    go stepBy stepsLeft total
      | stepsLeft == 0 = total
      | otherwise = go stepBy (stepsLeft - 1) (total + stepBy)
-- Fixing dividedBy
