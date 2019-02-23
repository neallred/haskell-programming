-- Exercises: Mood Swing
------------------------
-- 1. Mood
-- 2. Blah or Woot
-- 3. Woot is not a type, it is a data constructor.
-- 4. 
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

-- 5.
data Mood
  = Blah
  | Woot
  deriving (Show)

-- changeMood function above
-- Exercises: Find the Mistakes
-------------------------------
-- 1. not True && True -- False
-- 2. not (x == 6) -- depends on x
-- 3. (1 * 2) > 5 -- OK as is -- False
-- 4. "Merry" > "Happy" -- True
-- 5. ['1', '2', '3'] ++ "look at me!" -- "123look at me!"
-- Chapter Exercises
--------------------
-- 1. length :: [a] -> Int. It takes one List, and it evaluates to an Int
-- So what if your list has (maxBound :: Int) + 1 items?, would length be negative?
-- 2.
-- a) 5
-- b) 3
-- c) 2
-- d) 5
--
-- So, check this out.
-- λ> realLong = take (1234 + 2) $ repeat 2
-- λ> length realLong
-- 1236
-- λ> maxBound :: Int
-- 9223372036854775807
-- λ> realLong = take ((maxBound :: Int) + 2) $ repeat 2
-- λ> length realLong
-- 0
-- λ> realLong = take ((maxBound :: Int)) $ repeat 2
-- λ> length realLong
-- goes on and on, presumable it would terminate eventually?
--
-- 3.
-- 6 / 3 works because application happens to two numbers that can be fractionals
-- 6 / length [1, 2, 3] does not work because length returns an Int but / expects a fractional
-- 4.
fix4 = 6 `div` length [1, 2, 3]

-- 5. Bool, True
-- 6. Bool, False
-- awesome = ["Papuchon", "curry", ":)"]
-- also = ["Quake", "The Simons"]
-- allAwesome = [awesome, also]
-- 7. 
-- a) Will work, reduces to True
-- b) Will not work, Ints/Nums and Chars are different types
-- c) Will work, reduces to 5
-- d) Will work, reduces to False
-- e) Will not work, 9 is not a Boolean expression but && expects two Boolean expressions
-- 8.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

-- 9.
myAbs :: Integer -> Integer
myAbs x =
  if x < 0
    then x * (-1)
    else x

-- 10.
reorderTuple :: (a, b) -> (c, d) -> ((b, d), (a, c))
reorderTuple x y = ((snd x, snd y), (fst x, fst y))

-- Correcting Syntax
--------------------
-- 1.
x = (+)

f :: String -> Int
f xs = w `x` 1
  where
    w = length xs

-- 2.
-- \x = x
-- 3.
fst' (a, b) = a-- Match the function names to their types
------------------------------------------
-- 1. c)
-- 2. b)
-- 3. a)
-- 4. d)
