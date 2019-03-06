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
-- 2. "haha mrow 1"
-- 3. "woops mrow 2 mrow haha"
