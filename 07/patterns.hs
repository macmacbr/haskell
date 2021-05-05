module Patterns where

funcZ :: (Eq a, Num a) => a -> [Char]
funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

-- pal :: [Char] -> [Char]
pal :: Eq a => [a] -> [Char]
pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

pal' :: Eq a => [a] -> [Char]
pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs
