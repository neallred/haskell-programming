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
  (==) (Pair x1 y1) (Pair x2 y2) = x1 == x2 && y1 == y2

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
