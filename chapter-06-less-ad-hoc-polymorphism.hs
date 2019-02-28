import Data.List

data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

--  Trivial' /= Trivial' = True
data DayOfWeek
  = Mon
  | Tue
  | Weds
  | Thu
  | Fri
  | Sat
  | Sun

-- day of week and numerical day of month
data Date =
  Date DayOfWeek
       Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date weekday numday) (Date weekday' numday') =
    weekday == weekday' && numday == numday'

-- Exercises: Eq Instances
-- 1.
data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y

-- 2.
data TwoIntegers =
  Two Integer
      Integer

instance Eq TwoIntegers where
  (==) (Two x1 y1) (Two x2 y2) = x1 == y1 && x2 == y2

-- 3.
data StringOrInt
  = TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAString x) (TisAString y) = x == y
  (==) (TisAnInt x) (TisAnInt y) = x == y
  (==) _ _ = False

-- 4.
data Pair a =
  Pair a
       a

instance (Eq a) => Eq (Pair a) where
  (==) (Pair x1 y1) (Pair x2 y2) = x1 == x2 && y1 == y2

-- 5.
data Tuple a b =
  Tuple a
        b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x1 y1) (Tuple x2 y2) = x1 == x2 && y1 == y2

-- 6.
data Which a
  = ThisOne a
  | ThatOne a

instance (Eq a) => Eq (Which a) where
  (==) (ThisOne x) (ThisOne y) = x == y
  (==) (ThatOne x) (ThatOne y) = x == y
  (==) _ _ = False

-- 7.
data EitherOr a b
  = Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello y) = x == y
  (==) (Goodbye x) (Goodbye y) = x == y
  (==) _ _ = False

-- why didn't we need to make the type of the function we wrote require both type classes?
-- f :: (Num a, Fractional a) => a -> a -> a
--
-- Fraction already has Num as a superclass, so anything that is an instance of Fractional must be also be an instance of Num
--
-- Exercises: Will They Work?
-- 1. Will work, both lengths return an Int, and max only requires an Ord. Int has an instance of typeclass Ord. Will return 5
-- 2. Will work. will be a Num or an Int (with defaulted type classes). will be LT. 12 is less than 15. Int has an instance of Ord.
-- 3. will not work. [Char] can not be compared to Bool
-- 4. will work. Will return False. Int has an instance of Ord.
-- Chapter Exercises
-- Multiple choice
-- 1. The Eq class makes equality tests possible (c)
-- 2. The Ord is a subclass of Eq (b)
-- 3. a
-- 4. In `x = divMod 16 12` the type of `x` is a tuple
-- 5. The type class Integral includes Int and Integer numbers (a)
-- Does it typecheck?
---------------------
-- 1. No, Person does not have an instance of the Show typeclass. If provided one, it would wokr.
data CompileOnePerson =
  CompileOnePerson Bool
  deriving (Show)

printPerson :: CompileOnePerson -> IO ()
printPerson person = putStrLn (show person)

-- 2. Almost, it needs an Eq instance
data CompileTwoMood
  = CompileTwoBlah
  | CompileTwoWoot
  deriving (Show, Eq)

settleDown x =
  if x == CompileTwoWoot
    then CompileTwoBlah
    else x

-- 3.
-- a. Acceptable inputs are inhabitants of CompileTwoMood, namely CompileTwoBlah and CompileTwoWoot
-- b. Won't compile. No way for the numeric literal (Num a) to be a CompileTwoMood
-- c. Won't compile. Will throw an error that there's no instance of the Ord typeclass.
-- 4. yes, it compiles. s1 will not be fully applied, but that's ok!
type Subject = String

type Verb = String

type Object = String

data Sentence =
  Sentence Subject
           Verb
           Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"

s2 = Sentence "Julie" "loves" "dogs"

-- Given a datatype declaration, what can we do?
------------------------------------------------
data Rocks =
  Rocks String
  deriving (Eq, Show)

data Yeah =
  Yeah Bool
  deriving (Eq, Show)

data Papu =
  Papu Rocks
       Yeah
  deriving (Eq, Show)

-- 1. Doesn't compile. "chases" is a string, but needs to be (Rocks String) to be the Rocks that Papu needs. Similarly, True should be (Yeah True)
phew = Papu (Rocks "chases") (Yeah True)

-- 2. compiles.
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3. compiles.
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4. Doesn't compile. No instance for Ord for Papu.
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'
-- Match the types
------------------
-- 1. `i :: a` does not work. It says that it is nothing, no concrete type. The only thing that matches that is bottom/undefined
i :: Num a => a
i = 1

-- 2. Not going to work. b would make it more parametric than the term level expression
-- f :: Num a => a
f :: Float
f = 1.0

-- 3. Based on 2, it will work. The literal 1.0 can be a Fractional
-- f' :: Float
f' :: Fractional a => a
f' = 1.0

-- 4. Will work. Float is a subclass of RealFrac
-- f'' :: Float
f'' :: RealFrac a => a
f'' = 1

-- 5. It will work, though it is more constrained than it has to be.
-- freud :: a -> a
freud :: Ord a => a -> a
freud x = x

-- 6. It will work, though it is more constrained than it has to be.
-- freud' :: a -> a
freud' :: Int -> Int
freud' x = x

-- 7. Will not work. Int forces the type signature of the parameter to be an Int, because there's a match in the types
myX = 1 :: Int

-- sigmund :: a -> a
sigmund :: Int -> Int
sigmund x = myX

-- 8. Still won't work. Int is more specific than Num.
myX' = 1 :: Int

-- sigmund' :: Num a => a -> a
sigmund' :: Int -> Int
sigmund' x = myX'

-- 9. Works, but is more constrained than it has to be
-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10. Works. Ord a is less specific, but that's ok because no terms require a [Char] or Char
-- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11. Does not work. signifier doesn't strictly require [Char], but mySort, which it calls, does.
mySort :: [Char] -> [Char]
mySort = sort

-- signifier :: Ord a => [a] -> a
signifier :: [Char] -> Char
signifier xs = head (mySort xs)

-- Type-Kwon-Do Two: Electric Typealoo
-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk toEqualer x y = (toEqualer x) == y

-- 2.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x y = (f y) + fromInteger x
