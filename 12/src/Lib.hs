module Lib
    ( someFunc
    , loaded
    ) where

loaded :: Bool
loaded = True  
  
someFunc :: IO ()
someFunc = putStrLn "someFunc"
