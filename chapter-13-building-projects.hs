module Chapter13 where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersperse, sort)
import Data.Maybe (fromMaybe, isJust)
import System.Exit (exitSuccess)
import System.IO
import System.Random (randomRIO)
import Text.Read (readMaybe)

-- Intermission: Check your understanding
{-
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.MVar as MV
import Control.Exception (mask, try)
import Control.Monad (forever, when)
import Data.Bits
import Data.Bits.Bitwise (fromListBE)
import qualified Data.ByteString.Char8 as B
import Data.List.Split (chunksOf)
import qualified Data.Locator as DL
import qualified Data.Time.Clock.POSIX as PSX
import Database.Blacktip.Types
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FPC
import qualified Network.Info as NI
import qualified Safe
import System.IO.Unsafe (unsafePerformIO)
--}
-- Given this import list,
-- 1. forever and when are imported from Control.Monad
-- 2. Data.Bits and Database.Blacktip.Types are imported unqualified and in their entirety
-- 3. Importing Database.Blacktip.Types all the types and data constructors, and maybe some smart constructors
-- 4. a. CC is Control.Concurrent, MV is Control.Concurrent.MVar, FPC Filesystem.Path.CurrentOS
-- 4. b. FS.writeFile means Filesystem.writeFile
-- 4. c. forever comes from Control.Monad
twoo :: IO Bool
twoo = do
  c <- getChar
  c' <- getChar
  return (c == c')

-- Chapter Exercises
-- Hangman game logic
-- See hangman/src/Main.js
-- Modifying code
-- 1. See chapter-11-vignere-cipher.hs
-- 2.
-- 3.
acceptableChars = ['0' .. '9'] ++ ['a' .. 'z']

palify :: String -> String
palify = filter (`elem` acceptableChars) . map toLower

palindrome :: IO ()
palindrome =
  forever $ do
    line1 <- getLine
    if palify line1 == reverse (palify line1)
      then putStrLn "It's a palindrome!"
      else do
        putStrLn "Nope!"
        exitSuccess

-- 4.
type Name = String

type Age = Integer

data Person =
  Person Name
         Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise =
    Left $
    PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

getIntFromUser :: IO Integer
getIntFromUser = do
  userInput <- getLine
  case readMaybe userInput of
    Nothing -> do
      putStrLn "(integer input required, please try again)"
      getIntFromUser
    Just n -> return n

gimmePerson :: IO ()
gimmePerson
  -- a)
 = do
  putStrLn "Name, please:"
  name <- getLine
  putStrLn "Age (as a number), please:"
  age <- getIntFromUser
  -- b)
  case mkPerson name age of
    Right person
      -- c
     -> putStrLn $ "Yay! Successfully got a person: " ++ show person
    Left errorOccurred -> putStrLn $ "Error occurred: " ++ show errorOccurred
