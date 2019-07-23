module Chapter18 where

import Control.Applicative ((*>))
import Control.Monad ((>=>), join)

-- class Applicative m =>
--       Monad m
--   where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a
-- fmap' f xs = xs >>= return . f
-- join :: Monad m => m (m a) -> m a
bind :: Monad m => (a -> m b) -> m a -> m b
bind a_m_b m_a = join $ fmap a_m_b m_a

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' = putStrLn "blah" >> putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' = putStrLn "blah" *> putStrLn "another thing"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("h helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >> getLine >>= \name ->
    putStrLn ("y helo thar: " ++ name)

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("h helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >> getLine >>= \name ->
    putStrLn "age pls:" >> getLine >>= \age ->
      putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else twiceWhenEven (map (* 2) [1 .. x])
    -- else [x * x]

data Cow =
  Cow
    { name :: String
    , age :: Int
    , weight :: Int
    }
  deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
   in if n == "Bess" && w > 499
        then Nothing
        else Just c

-- This is pretty much how you'd write it in Elm
mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty -> weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>= \nammy ->
    noNegative age' >>= \agey ->
      noNegative weight' >>= \weighty -> weightCheck (Cow nammy agey weighty)

-- instance Monad Maybe where
--   return x = Just x
--   (Just x) >>= k = k x
--   Nothing >>= _ = Nothing
-- mkSphericalCow'' :: String
--                  -> 
type Founded = Int

type Coders = Int

data SoftwareShop =
  Shop
    { founded :: Founded
    , programmers :: Coders
    }
  deriving (Eq, Show)

data FoundedError
  = NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap m m' = do
  x <- m
  x' <- m'
  return (x x')

-- Exercise: Either Monad
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Sum a b) where
  (<>) (First x1) (First x2) = First (x1 <> x2)
  (<>) (Second x1) (Second x2) = Second (x1 <> x2)
  (<>) (First x) _ = First x
  (<>) _ (First x) = First x

instance (Monoid a, Monoid b) => Monoid (Sum a b) where
  mempty = First mempty
  mappend = (<>)

instance Functor (Sum a) where
  fmap f (First x) = First x
  fmap f (Second x) = Second (f x)

instance (Semigroup a) => Applicative (Sum a) where
  pure = Second
  (<*>) (First x1) (First x2) = First (x1 <> x2)
  (<*>) (First x1) (Second _) = First x1
  (<*>) (Second _) (First x1) = First x1
  (<*>) (Second f) (Second x) = Second (f x)

-- join :: Monad m => m (m a) -> m a
-- bind :: Monad m => (a -> m b) -> m a -> m b
-- bind a_m_b m_a = join $ fmap a_m_b m_a
instance (Semigroup a) => Monad (Sum a) where
  return = pure
  (>>=) (First x) _ = First x
  (>>=) m_a f = join $ fmap f m_a

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "
