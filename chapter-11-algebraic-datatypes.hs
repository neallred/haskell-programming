{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter11 where

import Data.Char
import Data.Int
import Data.List
import Data.List.Split

{-# ANN Price "HLint: ignore Use newtype instead of data" #-}

{-# ANN Size "HLint: ignore Use newtype instead of data" #-}

{-# ANN DogueDeBordeaux "HLint: ignore Use newtype instead of data"
        #-}

data PugType =
  PugData

data HuskyType a =
  HuskyData

data DogueDeBordeaux doge =
  DogueDeBordeaux doge

data Doggies a
  = Husky a
  | Mastiff a

-- Exercises: Dog Types
-- 1. Doggies is a type constructor
-- 2. :k Doggies is (Doggies a :: * -> *)
-- 3. :k Doggies String is (Doggies String :: *)
-- 4. :t Husky 10 is (Husky 10 :: Num a => Doggies a)
-- 5. :t Husky (10 :: Integer) is (Husky 10 :: Doggies Integer)
-- 6. :t Mastiff "Scooby Doo" is (Mastiff "Scooby Doo" :: Doggies String)
-- 7. DogueDeBordeaux is a data constructor and a type constructor. They do not
-- have to share the same name but that is a common Haskell convention.
-- 8. :t DogueDeBordeaux is (DogueDeBordeaux :: doge -> DogueDeBordeaux doge)
-- 9. :t DogueDeBordeaux "doggie!" is (DogueDeBordeaux :: DogueDeBordeaux String)
data Price =
  Price Integer
  deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsAreUs
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Size =
  Size Integer
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer
        Price
  | Plane Airline
          Size
  deriving (Eq, Show)

-- Exercises: Vehicles
myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir (Size 9001)

-- 1. :t myCar is (myCar :: Vehicle)
-- 2. 
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane x _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3.
getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x

-- partial function unless it can do something like return Maybe Manufacturer :(
-- 4.
-- It'll 'asplode. Runtime exception
-- Exercises: Cardinality
-- 1. cardinality of Pugtype is 1
-- 2. cardinality of Airline is 3
-- 3. cardinality of Int16 is 2^16 = 65536
-- 4. Int cardinality is BIIIIG. 9223372036854775807 + 1 + 9223372036854775808 = 18446744073709551616
-- Integer does not have an instance for bounded. It is a type class for the different Ints, right?
-- 5. 8 is how many bits of memory it can take. or 2 ^ 8 (one byte).
data Example =
  MakeExample
  deriving (Show) --

--
-- Exercises: For Example
-------------------------
-- 1. :t MakeExample is (MakeExample :: Example)
-- :t Example complains that there is no Example Data constructor (it isn't one, it is a type constructor. You could do :k Example, though)
-- 2. You can determine the type class instances. Currently only the Show typeclass is derived.
-- 3.
-- data AnotherExample =
--   AnotherMakeExample Int --
-- :t AnotherMakeExample now takes an Int as an argument: (AnotherMakeExample :: Int -> AnotherExample)
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (num, str) = num > 42

instance TooMany (Int, Int) where
  tooMany (field1, field2) = sum [field1, field2] > 42

-- creates an overlapping instances problem...
-- which causes an error trying to evaluate `tooMany (22 :: Int, 2 :: Int)
instance (Ord a, Num a) => TooMany (a, a) where
  tooMany (field1, field2) = (field1 * field2) > 42

newtype Goats =
  Goats Int
  deriving (Show, Eq, TooMany)

-- instance TooMany Goats where
--   tooMany (Goats n) = n > 43
--
-- instance TooMany Goats where
--   tooMany (Goats n) = tooMany n
-- Exercises: Pity the Bool
-- 1.
data BigSmall
  = Big Bool
  | Small Bool
  deriving (Eq, Show) --

-- Big Bool | Small Bool
-- Big 2 | Small 2
-- (Big * 2) | (Small * 2)
-- (1 * 2) | (1 * 2)
-- 2 | 2
-- 2 + 2
-- The cardinality of BigSmall is 4.
data NumberOrBool
  = Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show) --

-- Numba Int8 | BoolyBool Bool
-- 1 * 256 | 1 * 2
-- 256 + 2
-- 258
-- The cardinality of NumberOrBool is 258.
myNumba = Numba (-128)

-- the values will wrap
-- my ghci did not get an error about trying to use a 128 (which overflows) and then negate it...
-- Did something change in a GHCi version?
-- data Person =
--   MkPerson String
--            Int
--   deriving (Eq, Show)
-- jm = MkPerson "julie" 108
-- ca = MkPerson "chris" 16
-- 
-- namae :: Person -> String
-- namae (MkPerson s _ ) = s
data Person = Person
  { parsonName :: String
  , age :: Int
  } deriving (Eq, Show)

jm = Person "julie" 108

ca = Person "chris" 16

na = Person {parsonName = "na", age = 28}

data Fiction =
  FictionData
  deriving (Show)

data Nonfiction =
  NonfictionData
  deriving (Show)

data BookType
  = FictionBook Fiction
  | NonfictionBook Nonfiction
  deriving (Show)

type AuthorName = String

-- data Author =
--   Author (AuthorName, BookType)
data Author
  = Fiction AuthorName
  | Nonfiction AuthorName

--
-- data Expr
--   = Number Int
--   | Add Expr
--         Expr
--   | Minus Expr
--   | Mult Expr
--          Expr
--   | Divide Expr
--            Expr
-- type Number = Int
-- 
-- type Add = (Expr, Expr)
-- 
-- type Minus = Expr
-- 
-- type Mult = (Expr, Expr)
-- 
-- type Divide = (Expr, Expr)
-- 
-- type Expr = Either Number (Either Add (Either Minus (Either Mult Divide)))
-- but this causes error: Cycle in type synonym declarations
-- 
-- Exercises: How Does Your Garden Grow?
-- 1. Given
-- data FlowerType
--   = Gardenia
--   | Daisy
--   | Rose
--   | Lilac
--   deriving (Show)
-- 
-- type Gardener = String
-- 
-- data Garden =
--   Garden Gardener
--          FlowerType
--   deriving (Show)
--, what is the sum of products normal form of Garden?
type Gardener = String

data Gardenia =
  GardeniaData
  deriving (Show)

data Daisy =
  DaisyData
  deriving (Show)

data Rose =
  RoseData
  deriving (Show)

data Lilac =
  LilacData
  deriving (Show)

data FlowerType
  = FlowerGardenia Gardenia
  | FlowerDaisy Daisy
  | FlowerRose Rose
  | FlowerLilac Lilac

data Garden
  = Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener

data GuessWhat =
  Chickenbutt
  deriving (Eq, Show)

data Product a b =
  Product a
          b
  deriving (Eq, Show)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b = RecordProduct
  { pfirst :: a
  , psecond :: b
  } deriving (Eq, Show)

newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

data Farmhouse =
  Farmhouse NumCow
            NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

-- data Farmhouse'' = Product NumCow NumPig
-- can't do Farmhouse'' because then the data constructor of Farmhouse'' and the unary data constructor  Product of the type constructor Product would collide
newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmhouse =
  BigFarmhouse NumCow
               NumPig
               NumSheep
  deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type NameAnimals = String

type Age = Int

type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
  CowInfo NameAnimals
          Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo NameAnimals
          Age
          LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo NameAnimals
            Age
            PoundsOfWool
  deriving (Eq, Show)

data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

-- alternately
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

type Awesome = Bool

person :: Product NameAnimals Awesome
person = Product "Simon" True

data Twitter =
  Twitter
  deriving (Eq, Show)

data AskFm =
  AskFm
  deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer
  { os :: OperatingSystem
  , lang :: ProgLang
  } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

-- Exercise: Programmers
allProgrammers :: [Programmer]
allProgrammers =
  foldr (\x -> (++) (map (Programmer x) allLanguages)) [] allOperatingSystems

allProgrammersText :: String
allProgrammersText = intercalate "\n" $ map show allProgrammers

newtype Name =
  Name String
  deriving (Show)

newtype Acres =
  Acres Int
  deriving (Show)

data FarmerType
  = DairyFarmer
  | WheatFarmer
  | SoybeanFarmer
  deriving (Show)

data Farmer =
  Farmer Name
         Acres
         FarmerType
  deriving (Show)

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec = FarmerRec
  { name :: Name
  , acres :: Acres
  , farmerType :: FarmerType
  } deriving (Show)

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _ -> False

data Quantum
  = Yes
  | No
  | Both
  deriving (Eq, Show)

type Q = Quantum

quantSum1 :: Either Q Q
quantSum1 = Right Yes

quantSum2 :: Either Q Q
quantSum2 = Right No

quantSum3 :: Either Q Q
quantSum3 = Right Both

quantSum4 :: Either Q Q
quantSum4 = Left Yes

quantSum5 :: Either Q Q
quantSum5 = Left No

quantSum6 :: Either Q Q
quantSum6 = Left Both

quantProd1 :: (Q, Q)
quantProd1 = (Yes, Yes)

quantProd2 :: (Q, Q)
quantProd2 = (Yes, No)

quantProd3 :: (Q, Q)
quantProd3 = (Yes, Both)

quantProd4 :: (Q, Q)
quantProd4 = (No, Yes)

quantProd5 :: (Q, Q)
quantProd5 = (No, No)

quantProd6 :: (Q, Q)
quantProd6 = (No, Both)

quantProd7 :: (Q, Q)
quantProd7 = (Both, Yes)

quantProd8 :: (Q, Q)
quantProd8 = (Both, No)

quantProd9 :: (Q, Q)
quantProd9 = (Both, Both)

quantFlip1 :: Q -> Q
quantFlip1 Yes = Yes
quantFlip1 No = Yes
quantFlip1 Both = Yes

quantFlip2 :: Q -> Q
quantFlip2 Yes = Yes
quantFlip2 No = Yes
quantFlip2 Both = No

quantFlip3 :: Q -> Q
quantFlip3 Yes = Yes
quantFlip3 No = Yes
quantFlip3 Both = Both

quantFlip4 :: Q -> Q
quantFlip4 Yes = Yes
quantFlip4 No = No
quantFlip4 Both = Yes

quantFlip5 :: Q -> Q
quantFlip5 Yes = Yes
quantFlip5 No = Both
quantFlip5 Both = Yes

quantFlip6 :: Q -> Q
quantFlip6 Yes = No
quantFlip6 No = Yes
quantFlip6 Both = Yes

quantFlip7 :: Q -> Q
quantFlip7 Yes = Both
quantFlip7 No = Yes
quantFlip7 Both = Yes

quantFlip8 :: Q -> Q
quantFlip8 Yes = Both
quantFlip8 No = Yes
quantFlip8 Both = No

quantFlip9 :: Q -> Q
quantFlip9 Yes = Both
quantFlip9 No = Yes
quantFlip9 Both = Both

quantFlip10 :: Q -> Q
quantFlip10 Yes = Both
quantFlip10 No = No
quantFlip10 Both = Both

-- plus 17 more
-- Exponentiation in what order?
convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes = True
convert2 No = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes = False
convert4 No = True
convert4 Both = True

convert5 :: Quantum -> Bool
convert5 Yes = True
convert5 No = False
convert5 Both = False

convert6 :: Quantum -> Bool
convert6 Yes = False
convert6 No = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes = False
convert8 No = False
convert8 Both = False

-- Exercises: The Quad
data Quad
  = One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

-- 1.
equad :: Either Quad Quad
equad = undefined -- 4 + 4 = 8 possibilities (either is a sum type)

-- 2.
prodQuad :: (Quad, Quad)
prodQuad = undefined -- (,) means both at same time, or product so 4 * 4 = 16 possibilities

-- 3.
funcQuad :: Quad -> Quad
funcQuad = undefined -- a -> b means exponentation of form b ^ a, so 4 ^ 4 = 128 possibilities

-- 4.
prodTBool :: (Quad, Quad, Quad) -- 4 * 4 * 4 =  64 possibilities
prodTBool = undefined

-- 5.
gTwo :: Quad -> Quad -> Quad
gTwo = undefined

-- a -> b -> c  is (c ^ b) ^ a is c ^ (b * a)
-- so (4 ^ 4) ^ 4 = 4 ^ (4 * 4) = 4 ^ 16 = 2 ^ 2 ^ 16 = 2 ^ 32 = 4294967296
-- the number of possibilities of Int32!!
-- 6.
fTwo :: Bool -> Quad -> Quad
fTwo = undefined

-- (4 ^ 2) ^ 4
data Silly a b c d =
  MkSilly a
          b
          c
          d
  deriving (Show)

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

thyList = Cons 2 (Cons 3 (Cons 4 Nil))

thyList2 = 2 `Cons` (3 `Cons` (4 `Cons` Nil))

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+ 1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = inorder left ++ inorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f z (Node left a right) = foldTree f (foldTree f (f a z) left) right

-- Chapter Exercises
-- Multiple Choice
-- 1. Given
data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday --

-- we can say Weekday is a type with five data constructors
-- 2. and :t (f Friday = "Miller Time") is f :: Weekday -> String (c)
-- 3. Types defined with the data keyword must begin with a capital letter
-- 4. `g xs = xs !! (length xs - 1) delivers the final element of xs (if it has any, else it bottoms)
-- Ciphers
-- see cipher/src/LibCipher.hs
-- As-patterns
-- 1.
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf originalXs@(x:xs) originalYs@(y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf originalXs ys

-- 2.
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\original@(x:xs) -> (original, toUpper x : xs)) . words

--
-- Language exercises
-- 1.
capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs)
  | isAlphabets x = toUpper x : xs
  | otherwise = x : capitalizeWord xs

alphabets = ['a' .. 'z'] ++ ['A' .. 'Z']

isAlphabets = flip elem alphabets

-- 2.
capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph xs = go True xs
  where
    go _ "" = ""
    go pendingCapitalize (x:xs)
      | x == '.' = x : go True xs
      | pendingCapitalize && isAlphabets x = toUpper x : go False xs
      | otherwise = x : go pendingCapitalize xs

testCapitalizeParagraph =
  "every expression in haskell has a type which is determined at compile time. all the types composed together by function application have to match up. if they don't, the program will be rejected by the compiler. types become not only a form of guarantee, but a language for expressing the construction of programs."

capitalizeParagraph' :: String -> String
capitalizeParagraph' = intercalate "." . map capitalizeWord . splitOn "."

-- Phone exercise
-- See chapter-11-phone.hs (module Chapter11Phone)
--
-- Hutton's Razor
data Expr
  = Lit Integer
  | Add Expr
        Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add x y) = (printExpr x) ++ " + " ++ (printExpr y)
