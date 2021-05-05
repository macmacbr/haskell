-- wordnumber.hs
module WordNumber where
import Data.List (intersperse)

digitsStr :: [String]
digitsStr = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
digitsStrSize = length (digitsStr)

digitToWord :: Int -> String
digitToWord n 
  | (n < 0 || n >= digitsStrSize) = "???"
  | otherwise = (head . reverse . take (n+1)) $ digitsStr


digits :: Int -> [Int]
digits n
  | n < 10 = [ n ]
  | otherwise = digits (div n 10) ++ (digits (mod n 10)) 

stringator :: [String] -> String
stringator = concat . intersperse "-"

mapper :: [Int] -> [String]
mapper = map digitToWord

wordNumber :: Int -> String
wordNumber n = stringator . mapper $ digits n
