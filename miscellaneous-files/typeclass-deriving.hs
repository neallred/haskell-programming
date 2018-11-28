data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun

data Date =
  Date DayOfWeek Int


instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _   _   = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
    weekday == weekday'
   && dayOfMonth == dayOfMonth'

instance Show DayOfWeek where
  show Mon = "Mon"
  show Tue = "Tue"
  show Wed = "Wed"
  show Thu = "Thu"
  show Fri = "Fri"
  show Sat = "Sat"
  show Sun = "Sun"

instance Show Date where
  show (Date weekday dayOfMonth) = show ( show weekday ++ " " ++ show dayOfMonth )

instance Ord DayOfWeek where
  compare Mon Tue = LT
  compare Tue Wed = LT
  compare Wed Thu = LT
  compare Thu Fri = LT
  compare Fri Sat = LT
  compare Sat Sun = LT

  compare Mon Mon = EQ
  compare Tue Tue = EQ
  compare Wed Wed = EQ
  compare Thu Thu = EQ
  compare Fri Fri = EQ
  compare Sat Sat = EQ
  compare Sun Sun = EQ

-- syntastic suggests using newtype, b/c ?data? decreases laziness
-- data Identity a =
--   Identity a
-- 
-- instance (Eq a) => Eq (Identity a) where
--   (==) (Identity v) (Identity v') = v == v'

-- data TisAnInteger =
--   TisAn Integer
-- 
-- instance Eq TisAnInteger where
--   (==) (TisAn a) (TisAn a') = a == a'

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') = (a == a') && (b == b')

data StringOrInt
  = TisAnInt    Int
  | TisAString  String

instance Eq StringOrInt where
  (==) (TisAnInt _    ) (TisAString _)  = False
  (==) (TisAString _  ) (TisAnInt _)     = False
  (==) (TisAString a  ) (TisAString a') = a == a'
  (==) (TisAnInt a    ) (TisAnInt a')   = a == a'

data Pair a =
  Pair a a

instance (Eq a) => Eq (Pair a) where
  (==) (Pair a b) (Pair c d) = a == c && b == d

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple c d) = a == c && b == d

data Which a
  = ThisOne a
  | ThatOne a

instance (Eq a) => Eq (Which a) where
  (==) (ThisOne _) (ThatOne _) = False
  (==) (ThatOne _) (ThisOne _) = False
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'

data EitherOr a b
  = Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello _) (Goodbye _) = False
  (==) (Goodbye _) (Hello _) = False
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye a) (Goodbye a') = a == a'

add :: (Num a) => a -> a -> a
add x y = x + y

addWeird :: (Num a, Ord a) => a -> a -> a
addWeird x y =
  if x > 1
  then x + y
  else x
