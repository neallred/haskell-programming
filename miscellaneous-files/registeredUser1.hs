module RegisteredUser where

newtype Username =
  Username String

newtype AccountNumber =
  AccountNumber Integer

data User = 
  UnregisteredUser
  | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "ya'nt no one, bubs!"
printUser
  (RegisteredUser
    (Username "")
    (AccountNumber _)
  ) = putStrLn "missing username..."
printUser
  (RegisteredUser
    (Username u)
    (AccountNumber a)
  ) = putStrLn $ "User " ++ u ++ " with acount number " ++ show a


