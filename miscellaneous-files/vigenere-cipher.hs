module Vigenere where

import Data.Char


type StringHash = String
type StringMess = String
type CharHash = Char
type CharMess = Char
type CharHashCounter = Int

toVigy :: StringHash -> StringMess -> StringMess
toVigy "" mess = mess
toVigy _ "" = ""
toVigy unfilteredHash mess =
  let 
    hash = filter isAlpha unfilteredHash
      
  in
    goShiftify 0 hash mess

fromVigy :: StringHash -> StringMess -> StringMess
fromVigy "" mess = mess
fromVigy _ "" = ""
fromVigy unfilteredHash mess =
  let 
    hash = filter isAlpha unfilteredHash
      
  in
    goUnshiftify 0 hash mess

goShiftify :: CharHashCounter -> StringHash -> StringMess -> StringMess
goShiftify count hash "" = ""
goShiftify count hash (x:xs) =
  let
    isChar = isAlpha x
    nextCount = count + fromEnum isChar
    hashChar = charHashPicker count hash

  in
    (if isChar then shiftify hashChar x else x):(goShiftify nextCount hash xs)

charHashPicker :: CharHashCounter -> StringHash -> CharHash
charHashPicker count hash = hash !! (mod count (length hash))


shiftify :: CharHash -> CharMess -> Char
shiftify h m =
  chr .
  (+base) .
  (flip mod 25) .
  (shiftBy + ) .
  (subtract base) .
  fromEnum $ m
  where base = if (isUpper m) then 65 else 97
        shiftBy = getShiftBy h 


unshiftify :: CharHash -> CharMess -> Char
unshiftify h m =
  chr .
  (+base) .
  (flip mod 25) .
  (subtract shiftBy ) .
  (subtract base) .
  fromEnum $ m
  where base = if (isUpper m) then 65 else 97
        shiftBy = getShiftBy h 

goUnshiftify :: CharHashCounter -> StringHash -> StringMess -> StringMess
goUnshiftify count hash "" = ""
goUnshiftify count hash (x:xs) =
  let
    isChar = isAlpha x
    nextCount = count + fromEnum isChar
    hashChar = charHashPicker count hash

  in
    (if isChar then unshiftify hashChar x else x):(goUnshiftify nextCount hash xs)

getShiftBy :: Char -> Int
getShiftBy x = fromEnum x - (if isUpper x then 65 else 97) 
