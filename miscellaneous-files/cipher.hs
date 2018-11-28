module Caesar where

import Data.Char

makeCipher :: Int -> (String -> String)
makeCipher x = cipherMessage x

cipherMessage :: Int -> String -> String
cipherMessage shiftBy message =
  let shiftify x =
        if charIsAlpha
           then
            chr .
            (if charIsUpper then (+65) else (+97)) .
            (flip mod 25) .
            (shiftBy +) .
            (if charIsUpper then subtract 65 else subtract 97) .
            ord $ x
           else x
        where charIsUpper = (isUpper x)
              charIsAlpha = (elem x (['a'..'z'] ++ ['A'..'Z']))
  in
  map shiftify message 

decipherMessage :: Int -> String -> String
decipherMessage shiftBy message =
  let shiftify x =
        if charIsAlpha
           then
            chr .
            (if charIsUpper then (+65) else (+97)) .
            (flip mod 25) .
            (subtract shiftBy ) .
            (if charIsUpper then subtract 65 else subtract 97) .
            ord $ x
           else x
        where charIsUpper = (isUpper x)
              charIsAlpha = (elem x (['a'..'z'] ++ ['A'..'Z']))
  in
  map shiftify message 


-- upperBounds = (65, 90)
-- lowerBounds = (97, 122)
--A 65, Z 90
--a 97, z 122

-- succ, pred

