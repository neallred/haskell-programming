module Spec where

import LibCipher
import Test.QuickCheck

genPropData :: Int -> String -> String -> Property
genPropData shiftBy pad message =
  shiftBy < 100 && shiftBy > -100 ==> propGoodCipher shiftBy pad message

propGoodCipher :: Int -> String -> String -> Bool
propGoodCipher shiftBy pad message =
  let (decode, encode) = biumvirate shiftBy pad
   in (decode . encode $ message) == message

main :: IO ()
main = quickCheck (withMaxSuccess 10000 propGoodCipher)
