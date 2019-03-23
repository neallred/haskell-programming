module Chapter9Cipher where

import Data.Char

-- ord
-- chr
minUppercase = ord 'A' :: Int

maxUppercase = ord 'Z' :: Int

minLowercase = ord 'a' :: Int

maxLowercase = ord 'z' :: Int

shiftLetter :: Int -> Char -> Char
shiftLetter 0 letter = letter
shiftLetter shiftAmount letter =
  let letterInt = ord letter
      isLower = elem letterInt [minLowercase .. maxLowercase]
      isUpper = elem letterInt [minUppercase .. maxUppercase]
      isUnshiftable = not isLower && not isUpper
   in if isUnshiftable
        then letter
        else let effectiveMin =
                   if isLower
                     then minLowercase
                     else minUppercase
                 effectiveMax =
                   if isLower
                     then maxLowercase
                     else maxUppercase
              in chr $
                 effectiveMin +
                 mod
                   (letterInt + shiftAmount - effectiveMin)
                   (effectiveMax - (effectiveMin - 1))

makeCaesar :: Int -> String -> String
makeCaesar x = map (shiftLetter x)

makeUncaesar :: Int -> String -> String
makeUncaesar x = map (shiftLetter (negate x))

biumvirate :: Int -> ((String -> String), (String -> String))
biumvirate x = (makeCaesar (negate (abs x)), makeCaesar (abs x))

(uncaesar13, caesar13) = biumvirate 13
