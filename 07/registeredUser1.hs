-- registeredUser1.hs
module RegisteredUser where

newtype Username =
  Username String

newtype AccountNumber =
  AccountNumber Integer

data User =
    UnregisteredUser
  | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser =
  putStrLn "UnregisteredUser"

printUser (RegisteredUser 
            (Username name) 
            (AccountNumber acc)) =
  putStrLn $ name ++ " " ++ show acc

