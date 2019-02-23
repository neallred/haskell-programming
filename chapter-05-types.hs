{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

--
-- Exercises: Type Matching
---------------------------
-- 1, 2.
-- not :: Bool -> Bool
-- length :: [a] -> Int
-- concat :: [[a]] -> [a]
-- head :: [a] -> a
-- (<) :: (Ord a) => a -> a -> Bool
-- Exercises: Type Arguments
----------------------------
-- 1. Char -> Char -> Char
-- 2. Char
-- 3. Num b => b
-- 4. Double
-- 5. [Char]
-- 6. Eq b => b -> [Char]
-- 7. (Num a, Ord a) => a
-- 8. (Num a, Ord a) => a
-- 9. Integer
-- Exercises: Parametricity
---------------------------
-- 1. 
doNothing :: a -> a
doNothing x = x

--doNothing x = x + 1
-- We do not know it is a number, or ANY particular type. So it is impossible to say what typeclasses apply to it, and the only thing that will ever work is to return the exact same thing.
-- 2.
doLittle :: a -> a -> a
doLittle x _ = x
doLittle _ y = y

-- ditto explanation in 1
-- 3.
returnSnd :: a -> b -> b
returnSnd x y = y

-- Behavior always returns y argument unchanged.
-- Exercises: Apply Yourself
----------------------------
-- 1. myConcat :: String -> String
-- 2. myMult :: (Fractional a) => a -> a -- due to division
-- 3. myTake :: Int -> String
-- 4. myCom :: Int -> Bool
-- 5. myAlph :: Char -> Bool
-- Chapter Exercises
--------------------
-- Multiple Choice
------------------
-- 1. c)
-- 2. a)
-- 3. b)
-- 4. c)
-- Determine the type
---------------------
-- 1.
-- a) Num a => a
-- b) Num a => (a, [char])
-- c) (Integer, [Char])
-- d) Bool
-- e) Int
-- f) Bool
-- 2.
type2x = 5

type2y = type2x

type2w = type2y * 10

-- type of w is Num a => a
-- Nothing about the instruction 5 restricts it from being any type of Num. Neither does multiplication as all Num types support multiplication
-- 3.
type3x = 5

type3y = type3x + 5

type3z type3y = type3y * 10

-- There is shadowing going on, so type3z is actually a function.
-- Num a => a -> a
-- 4.
type4x = 5

type4y = type4x + 5

type4f = 4 / type4y

-- Though it is named f, it is a value not a function. It divides, so it needs to be fracitonal
-- So Fractional a => a
-- 5. [Char]
-- Does it compile?
-------------------
-- 1.
compile1BigNum = (^) 5 $ 10

-- compile1Wahoo = compile1BigNum $ 10
-- Breaks because (^) is already fully applied
-- 2.
-- No issues. It is valid to pass functions around
compile2x = print

compile2y = print "woohoo!"

compile2z = compile2x "hello world!"

-- 3.
compile3a = (+)

compile3b = 5

-- compile3c = compile3b 10
-- compile3d = compile3c 200
compile3cCorrected = compile3a compile3b 10

compile3cCorrected' = compile3a 10

compile3dCorrected = compile3a 200

-- 4.
compile4a = 12 + compile4b

-- compile4b = 10000 * compile4c
compile4b = 10000 * compile4cCorrected

compile4cCorrected = 123 -- c is an example value

-- Type variable or specific type constructor?
----------------------------------------------
-- 1.
--    [0] constrained polymorphic type variable
--    [1] fully polymorphic type variable
--    [2] concrete type constructor
--    [3] concrete type constructor
-- 2.
--    [0] fully polymorphic type variable
--    [1] concrete type constructor
--    [2] concrete type constructor
-- 3.
--    [0] fully polymorphic type variable
--    [1] constrained polymorphic type variable
--    [2] concrete type constructor
-- 4.
--    [0] fully polymorphic type variable
--    [1] fully polymorphic type variable
--    [2] concrete type constructor
-- Write a type signature
-------------------------
-- 1.
functionH :: [a] -> a
functionH (x:_) = x

-- 2.
functionC :: (Ord a) => a -> a -> Bool
functionC x y =
  if (x > y)
    then True
    else False

-- term level is alpha equivalent to (>)
-- 3.
functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function
-----------------------------------
writeTheFunctionExample :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
writeTheFunctionExample xtoY yToZ _ (a, x) = (a, yToZ . xtoY $ x)

writeTheFunction1 :: a -> a
writeTheFunction1 x = x

writeTheFunction2 :: a -> b -> a
writeTheFunction2 x y = x

writeTheFunction3 :: b -> a -> b
writeTheFunction3 x y = x

-- 2 and 3 are alpha equivalent in their types. Because of that and that there is only one implementation for 2, the implementations must be the same.
writeTheFunction4 :: a -> b -> b
writeTheFunction4 x y = y

-- 5.
writeTheFunction5a :: [a] -> [a]
writeTheFunction5a x = x ++ x

writeTheFunction5b :: [a] -> [a]
writeTheFunction5b x = x

writeTheFunction5c :: [a] -> [a]
writeTheFunction5c (x1:x2:xs) = xs
writeTheFunction5c (x:xs) = xs
writeTheFunction5c x = x

writeTheFunction6 :: (b -> c) -> (a -> b) -> a -> c
writeTheFunction6 = (.)

writeTheFunction7 :: (a -> c) -> a -> a
writeTheFunction7 f x = x

writeTheFunction8 :: (a -> b) -> a -> b
writeTheFunction8 f x = f x
