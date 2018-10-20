-- Exercises: Scope
-------------------
-- 1. Is y in scope for z?
-- Prelude> let x = 5
-- Prelude> let y = 7
-- Prelude> let z = x * y
--
-- Yes

-- 2. Is h in scope for g?
-- Prelude> let f = 3
-- Prelude> let g = f + h
--
-- No

-- 3. Is y in scope for z?
-- Prelude> let x = 5
-- Prelude> let y = 7
-- Prelude> let z = x * y
--
-- Yes

-- 4.
-- area d = pi * (r * r)
-- r = d / 2
--
-- Problem: r function is not tied r's in area function

-- 5.
-- area d = pi * (r * r)
--     where r = d / 2
--
-- area can execute

-- Exercises: Syntax Errors
---------------------------
-- 1. ++ [1,2,3] [4,5,6]
-- -> (++) [1,2,3] [4,5,6]

-- 2. '<3' ++ ' Haskell'
-- -> "<3" ++ " Haskell"

-- 3. concat ["<3", " Haskell"]
-- OK

-- Chapter Exercises
--------------------

-- Reading syntax
-- 1.
-- a) concat [[1,2,3], [4,5,6]]
-- OK

-- b) ++ [1,2,3] [4,5,6]
-- -> (++) [1,2,3] [4,5,6]

-- c) (++) "hello" " world"
-- Ok

-- d) ["hello" ++ " world]
-- -> ["hello" ++ " world"]

-- e) 4 !! "hello"
-- -> "hello" !! 4

-- f) "hello" !! 4
-- OK

-- g) take "4 lovely"
-- -> take 4 "lovely"

-- h) take 3 "awesome"
-- OK

-- 2.
-- Input | Output
-----------------
-- a)    | d)
-- b)    | c)
-- c)    | e)
-- d)    | a)
-- e)    | b)

-- Building functions
-- 1,2.

-- a)
addExclamation :: String -> String
addExclamation x
  = x ++ "!"

-- b)
takeI4 :: String -> String
takeI4 x =
  [x !! 4]

-- c)
takeLast8 :: String -> String
takeLast8 x =
  drop (length x - 8) x

-- 3. 
thirdLetter :: String -> Char
thirdLetter x =
  x !! 2

-- 4.
letterIndex :: Int -> Char
letterIndex x =
  "Curry is awesome" !! x

-- 5.
rvrs :: [a] -> [a]
rvrs [] =
  []
rvrs x  = 
  drop (length x - 1) x ++ rvrs (take (length x - 1) x)

-- 6.
-- See reverse.hs
