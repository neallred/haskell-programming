-- greetIfCool3.hs

module GreetIfCool3 where
greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
       True -> putStrLn "eyyyyy. What's shakin'?"
       False -> putStrLn "pshhht."
  where cool = coolness == "downright frosty yo"

