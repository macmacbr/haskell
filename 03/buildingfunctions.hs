-- buildingfunctions

module Buildingfunctions where

tt :: String -> String
tt s = tail s

bang :: String -> String
bang s = (++) s "!"

rvrs :: String -> String
rvrs x = c ++ " " ++ b ++ " " ++ a
  where a = take 5 x
        b = take 2 $ drop 6 x 
        c = drop 9 x

main :: IO ()
main = print (rvrs "Curry is awesome")

