-- print1.hs
module Print1 where

mainPrint1 :: IO ()
mainPrint1 = putStrLn "hello world!"

-- print2.hs
module Print2 where

mainPrint2 :: IO ()
mainPrint2 = do
  putStrLn "Count to four for me:"
  putStr "on, two"
  putStr ", three, and"
  putStrLn " four!"

-- print3.hs
module Print3 where

myGreeting :: String
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

mainPrint3 :: IO ()
mainPrint3 = do
  putStrLn myGreeting
  putStrLn secondGreeting
    where secondGreeting = 
            concat [hello, " ", world]
module TopOrLocal where

topLevelFunction :: Integer -> Integer
topLevelFunction x =
  x + woot where
             woot :: Integer
             woot = 10

topLevelValue :: Integer
topLevelValue = 5

-- print3flipped.hs

module Print3Flipped where

myGreeting :: String
myGreeting =(++) "hello" " world!"

hello :: String
hello = "hello"


world :: String
world = "world!"

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where secondGreeting =
        (++) hello ((++) " " world)

--print3Broken.hs

-- module Print3Broken where

-- printSecond :: String -> IO ()
-- printSecond greeting = do
--   putStrLn greeting
-- 
-- main :: IO ()
-- main = do
--   putStrLn greeting
--   printSecond greeting
--   where greeting = "Yarrrrr"
