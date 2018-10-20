module Reverse where

rvrs :: String -> String
rvrs "" =
  ""
rvrs x  = 
  drop (length x - 1) x ++ rvrs (take (length x - 1) x)


main :: IO ()
main = print $ rvrs "Curry is asesome"
