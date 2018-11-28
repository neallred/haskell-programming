{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import           Data.Int

data Price =
  Price Integer deriving (Eq, Show)

data Size =
  Size Integer deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

trummPlane  = Plane CatapultsR'Us (Size 234)
trummMobile = Car Mazda (Price 399393)

myCar    = Car Mini (Price 14000) -- Type is Vehicle
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir (Size 234)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane x = not $ isCar x

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Plane _ _ ) = undefined
getManu (Car m _)    = m

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

newtype Cows =
  Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42


class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42


newtype IntStringPair =
  IntStringPair (Int, String) deriving (Eq, Show)

instance TooMany IntStringPair where
  tooMany (IntStringPair (i, s)) = i > 42

newtype GoatPair =
  GoatPair (Int, Int) deriving (Eq, Show)

instance TooMany GoatPair where
  tooMany (GoatPair (p1, p2)) = p1 + p2 > 42

newtype AverageGoats =
  AverageGoats [Int] deriving (Eq, Show)

instance TooMany AverageGoats where
  tooMany (AverageGoats gs) = (fromIntegral (sum gs)) / (fromIntegral (length gs)) > 42

-- instance TooMany MysteryPair where
--   tooMany (MysteryPair (p1, p2)) = p1 + p2 > 42

data NumberOrBool
  = Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

data MkPerson
  = MkPerson String Int
  deriving (Eq, Show)

jm = MkPerson "julie" 108
ca = MkPerson "chris" 16

namae :: MkPerson -> String
namae (MkPerson s _) = s

-- data Person =
--   Person { name :: String
--   , age :: Int
--   }

-- data Fiction = Fiction deriving Show
--
-- data Nonfiction = Nonfiction deriving Show

-- data BookType = FictionBook Fiction
--               | NonfictionBook Nonfiction
--               deriving Show

type AuthorName = String

-- data Author = Author (AuthorName, BookType)

data Author
  = Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)

data Expr
  = Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr Expr
  | Divide Expr Expr

newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)


data Farmhouse =
  Farmhouse NumCow NumPig
  deriving (Eq, Show)

-- Couldn't get this working, couldn't find how to import the package.
-- type Farmhouse' = Product NumCow NumPig
--
-- newtype NumSheep =
--   NumSheep Int
--   deriving (Eq, Show)
--
-- data BigFarmhouse =
--   BigFarmhouse NumCow NumPig NumSheep
--   deriving (Eq, Show)
--
-- type BigFarmhouse' =
--   Product NumCow (Product NumPig NumSheep)
--
-- type Name = String
-- type Age = Int
-- type LovesMud = Bool
--
-- type PoundsOfWool = Int
--
-- data CowInfo =
--   CowInfo Name Age
--              deriving (Eq, Show)
--
-- data PigInfo =
--   PigInfo Name Age LovesMud
--
-- data SheepInfo =
--   SheepInfo Name Age PoundsOfWool
--                deriving (Eq, Show)
--
-- data Animal
--   = Cow CowInfo
--   | Pig PigInfo
--   | Sheep SheepInfo
--   deriving (Eq, Show)
--
-- type Animal' =
--   Sum CowInfo (Sum PigInfo SheepInfo)

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show, Enum)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show, Enum)

data Programmer
  = Programmer { os   :: OperatingSystem
               , lang :: ProgLang }
  deriving (Eq, Show)

allProgrammers =
  [Programmer x y | x <- [GnuPlusLinux ..], y <- [Haskell ..]]

-- BAD!
-- partialAf = Programmer { os = GnuPlusLinux }

data ThereYet =
  There Float Int Bool
  deriving (Eq, Show)

-- who needs a "builder pattern"?
notYet :: Int -> Bool -> ThereYet
notYet = There 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yusssss :: ThereYet
yusssss = notQuite False


newtype Name  = Name String deriving (Eq, Show)
newtype Acres = Acres Int deriving (Eq, Show)

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving (Eq, Show)

data Farmer =
  Farmer Name Acres FarmerType
  deriving (Eq, Show)

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) =
  True
isDairyFarmer _ =
  False

data FarmerRec =
  FarmerRec { name       :: Name
            , acres      :: Acres
            , farmerType :: FarmerType }
            deriving Show

isDaryFarmerRec :: FarmerRec -> Bool
isDaryFarmerRec farmer =
  case farmerType farmer of
       DairyFarmer -> True
       _           -> True

data Quantum
  = Yes
  | No
  | Both
  deriving (Eq, Show)

-- 3 + 3

quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Yes

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Both

quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both


quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)

quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)

quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)

quantProd4 :: (Quantum, Quantum)
quantProd4 = (No, Yes)

quantProd5 :: (Quantum, Quantum)
quantProd5 = (No, No)

quantProd6 :: (Quantum, Quantum)
quantProd6 = (No, Both)

quantProd7 :: (Quantum, Quantum)
quantProd7 = (Both, Yes)

quantProd8 :: (Quantum, Quantum)
quantProd8 = (Both, No)

quantProd9 :: (Quantum, Quantum)
quantProd9 = (Both, Both)



quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes  = Yes
quantFlip1 No   = Yes
quantFlip1 Both = Yes

quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes  = Yes
quantFlip2 No   = Yes
quantFlip2 Both = No

quantFlip3 :: Quantum -> Quantum
quantFlip3 Yes  = Yes
quantFlip3 No   = Yes
quantFlip3 Both = Both

quantFlip4 :: Quantum -> Quantum
quantFlip4 Yes  = Yes
quantFlip4 No   = No
quantFlip4 Both = Yes

quantFlip5 :: Quantum -> Quantum
quantFlip5 Yes  = Yes
quantFlip5 No   = Both
quantFlip5 Both = Yes

quantFlip6 :: Quantum -> Quantum
quantFlip6 Yes  = No
quantFlip6 No   = Yes
quantFlip6 Both = Yes

quantFlip7 :: Quantum -> Quantum
quantFlip7 Yes  = Both
quantFlip7 No   = Yes
quantFlip7 Both = Yes

quantFlip8 :: Quantum -> Quantum
quantFlip8 Yes  = Both
quantFlip8 No   = Yes
quantFlip8 Both = No

quantFlip9 :: Quantum -> Quantum
quantFlip9 Yes  = Both
quantFlip9 No   = No
quantFlip9 Both = No

quantFlip10 :: Quantum -> Quantum
quantFlip10 Yes  = Both
quantFlip10 No   = No
quantFlip10 Both = Both

-- quantFlip11 :: Quantum -> Quantum
-- quantFlip11 Yes  = Yes
-- quantFlip11 No   = Yes
-- quantFlip11 Both = Yes
--
-- quantFlip12 :: Quantum -> Quantum
-- quantFlip12 Yes  = Yes
-- quantFlip12 No   = Yes
-- quantFlip12 Both = Yes
--
-- quantFlip13 :: Quantum -> Quantum
-- quantFlip13 Yes  = Yes
-- quantFlip13 No   = Yes
-- quantFlip13 Both = Yes
--
-- quantFlip14 :: Quantum -> Quantum
-- quantFlip14 Yes  = Yes
-- quantFlip14 No   = Yes
-- quantFlip14 Both = Yes
--
-- quantFlip15 :: Quantum -> Quantum
-- quantFlip15 Yes  = Yes
-- quantFlip15 No   = Yes
-- quantFlip15 Both = Yes
--
-- quantFlip16 :: Quantum -> Quantum
-- quantFlip16 Yes  = Yes
-- quantFlip16 No   = Yes
-- quantFlip16 Both = Yes
--
-- quantFlip17 :: Quantum -> Quantum
-- quantFlip17 Yes  = Yes
-- quantFlip17 No   = Yes
-- quantFlip17 Both = Yes
--
-- quantFlip18 :: Quantum -> Quantum
-- quantFlip18 Yes  = Yes
-- quantFlip18 No   = Yes
-- quantFlip18 Both = Yes
--
-- quantFlip19 :: Quantum -> Quantum
-- quantFlip19 Yes  = Yes
-- quantFlip19 No   = Yes
-- quantFlip19 Both = Yes
--
-- quantFlip20 :: Quantum -> Quantum
-- quantFlip20 Yes  = Yes
-- quantFlip20 No   = Yes
-- quantFlip20 Both = Yes
--
-- quantFlip21 :: Quantum -> Quantum
-- quantFlip21 Yes  = Yes
-- quantFlip21 No   = Yes
-- quantFlip21 Both = Yes
--
-- quantFlip22 :: Quantum -> Quantum
-- quantFlip22 Yes  = Yes
-- quantFlip22 No   = Yes
-- quantFlip22 Both = Yes
--
-- quantFlip23 :: Quantum -> Quantum
-- quantFlip23 Yes  = Yes
-- quantFlip23 No   = Yes
-- quantFlip23 Both = Yes
--
-- quantFlip24 :: Quantum -> Quantum
-- quantFlip24 Yes  = Yes
-- quantFlip24 No   = Yes
-- quantFlip24 Both = Yes
--
-- quantFlip25 :: Quantum -> Quantum
-- quantFlip25 Yes  = Yes
-- quantFlip25 No   = Yes
-- quantFlip25 Both = Yes
--
-- quantFlip26 :: Quantum -> Quantum
-- quantFlip26 Yes  = Yes
-- quantFlip26 No   = Yes
-- quantFlip26 Both = Yes
--
-- quantFlip27 :: Quantum -> Quantum
-- quantFlip27 Yes  = Yes
-- quantFlip27 No   = Yes
-- quantFlip27 Both = Yes

convert1 :: Quantum -> Bool
convert1 Yes  = True
convert1 No   = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes  = True
convert2 No   = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes  = True
convert3 No   = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes  = False
convert4 No   = True
convert4 Both = True

convert5 :: Quantum -> Bool
convert5 Yes  = False
convert5 No   = False
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes  = False
convert6 No   = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes  = True
convert7 No   = False
convert7 Both = False

convert8 :: Quantum -> Bool
convert8 Yes  = False
convert8 No   = False
convert8 Both = False


data List a = Nil | Cons a (List a)
            deriving (Eq, Show)

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a ( BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a
  => a
  -> BinaryTree a
  -> BinaryTree a
insert' b Leaf =
  Node Leaf b Leaf
insert' b (Node l a r)
  | b == a = Node l a r
  | b < a  = Node (insert' b l) a r
  | b > a  = Node l a (insert' b r)


gtde 0 = 0
gtde 1 = 1
gtde n = 2 * (n - 1) + gtde (n - 1)


mapTree :: (a -> b)
  -> BinaryTree a
  -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node l a r) =
  Node (mapTree f l) (f a) (mapTree f r)

preorder :: BinaryTree a -> [a]
preorder Leaf         = []
preorder (Node l a r) = [a] ++ (preorder l) ++ (preorder r)

inorder :: BinaryTree a -> [a]
inorder Leaf         = []
inorder (Node l a r) = (preorder l) ++ [a] ++ (preorder r)

postorder :: BinaryTree a -> [a]
postorder Leaf         = []
postorder (Node l a r) = (preorder r)  ++ (preorder l) ++ [a]

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2,1,3]
     then putStrLn "R"
     else putStrLn "L"

testInorder :: IO ()
testInorder =
  if inorder testTree == [1,2,3]
     then putStrLn "R"
     else putStrLn "L"

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [3,1,2]
     then putStrLn "R"
     else putStrLn "L"

foldTree :: (a -> b -> b)
  -> b
  -> BinaryTree a
  -> b
foldTree f x Leaf         = x
foldTree f x (Node l a r) = f a ((foldTree f (foldTree f x r) l))

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t



