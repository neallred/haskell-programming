-- Exercises: Comprehension check
---------------------------------

-- 1.
half x = x / 2
-- let half x = x / 2

square x = x * x
-- let square x = x * x

-- 2.
calculateCircleAreaBad r = 3.14 * (r * r)

-- 3.
calculateCircleAreaGood r = pi * (r * r)

-- Exercises: Comprehension check
---------------------------------

-- 1.
-- a) 8 + 7 * 9 == 71
-- b) (8 + 7) * 9 == 135
--
-- Different
--
-- 2.
-- a) perimeter x y = (x * 2) + (y * 2)
-- b) perimeter x y = x * 2 + y * 2
--
-- Same
--
-- 3.
-- a) f x = x / 2 + 9
-- b) f x = x / (2 + 9)
--
-- Different

-- Exercises: Heal the Sick
---------------------------
-- 1.
area x = 3.14 * (x * x)

-- 2.
double x = x * 2

-- 3.
hx = 7
hy = 10
hf = hx + hy

-- Exercises: A Head Code
-------------------------
-- 1. let x = 5 in x
-- -> 5
--
-- 2. let x = 5 in x * x
-- -> 25
--
-- 3. let x = 5; y = 6 in x * y
-- -> 30
--
-- 4. let x = 3; y = 1000 in x + 3
-- -> 6
--
-- rewrite as where statements:
-- 1. let x = 3; y = 1000 in x * 3 + y

where1      = x * 3 + y
    where x = 3
          y = 1000

-- 2. let y = 10; x = 10 * 5 + y in x * 5

where2      = x * 5
    where x = 10 * 5 + y
          y = 10

-- 3. let x = 7
--        y = negate x
--        z = y * 10
--    in z / x + y

where3      = z / x + y
    where x = 7
          y = negate x
          z = y * 10

-- Chapter Exercises
--------------------

-- Parenthesization
-------------------
-- 1. 2 + 2 * 3 - 1
parenthesization1 = 2 + (2 * 3) - 1

-- 2. (^) 10 $ 1 + 1
parenthesization2 = (^) 10 (1 + 1)

-- 3. 2 ^ 2 * 4 ^ 5 + 1
parenthesization3 = (2 ^ 2) * (4 ^ 5) + 1

-- Equivalent Expressions
-------------------------
-- 1. 1 + 1 == 2
-- True

-- 2. 10 ^ 2 == 10 + 9 * 10
-- True

-- 3. 400 - 37 == (-) 37 400
-- False

-- 4. 100 `div` 3 == 100 / 3
-- False

-- 5. 2 * 5 + 18 = 2 * (5 + 18)
-- False

-- More fun with functions
--------------------------
-- 1.

z = 7
x = y ^ 2
waxOn = x * 5
y = z + 8

-- 10 + waxOn
-- -> 1135

-- (+10) waxOn
-- -> 1135

-- (-) 15 waxOn
-- -> (-1110)

-- (-) waxOn 15
-- -> 1110

-- 2.
-- 3.
-- triple x = x * 3
-- triple waxOn
-- -> 3375

-- 4.
whereWaxOn  = x * 5
    where z = 7
          x = y ^ 2
          y = z + 8

-- 5.
triple x = x * 3

-- 6.
waxOff x = triple x

-- 7.
waxOff' x = (triple x + 10) ^ (2)
waxOff'' = triple . triple . triple
