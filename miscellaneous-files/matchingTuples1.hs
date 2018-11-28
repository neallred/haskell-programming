module TupleFunctions where

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt = uncurry (+)

fst3 :: (a, b, c) -> a
fst3 (x,_,_) = x

third3 :: (a,b,c) -> c
third3 (x,y,z) = z

f :: (a,b,c)
  -> (d,e,f)
  -> ((a, d), (c, f))
f (q,r,s) (t,u,v) = ((q,t),(s,v))

funcZ x =
  case x + 1 == 1 of
       True -> "AWESOME"
       False -> "wut"

pal xs =
  case xs == reverse xs of
       True -> "is a palindrome"
       False -> "is not a palindrome"

pal' xs =
  case y of
       True -> "is"
       False -> "is not!"
  where y = xs == reverse xs

-- functionC x y = if (x > y) then x else y

functionC x y =
  case z of
       True -> x
       False -> y
  where
    z = (x > y)

-- ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2 n =
 case isEven of
      True -> n + 2
      False -> n
  where
    isEven = even n

nums x =
 case compare x 0 of
      LT -> -1
      EQ -> 0
      GT -> 1

data Employee
  = Coder
  | Manager
  | Veep
  | CEO
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++
  " is the boss of " ++
  show e'

employeeRank ::
  (  Employee
  -> Employee
  -> Ordering
  )
  -> Employee
  -> Employee
  -> IO ()
employeeRank f e e' =
  case f e e' of
       GT -> reportBoss e e'
       EQ -> putStrLn "Neither employee is the boss"
       LT -> (flip reportBoss) e e'

codersRule :: Employee
  -> Employee
  -> Ordering
codersRule Coder Coder = EQ
codersRule Coder _     = GT
codersRule _     Coder = LT
codersRule e     e'    =
  compare e e'

dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: (Num a) => a -> a
oneIsOne = dodgy 1

oneIsTwo :: (Num a) => a -> a
oneIsTwo = (flip dodgy) 2

-- 2. dodgy 1 1      11
-- 3. dodgy 2 2      22
-- 4. dodgy 1 2      21
-- 5. dodgy 2 1      12
-- 6. oneIsOne 1     1 + y * 10
-- 7. oneIsOne 2     1 + y * 10
-- 8. oneIsTwo 1     x + 10
-- 9. oneIsTwo 2     x + 20
-- 10. oneIsOne 3    3 + y * 10
-- 11. oneIsTwo 3    x + 30
