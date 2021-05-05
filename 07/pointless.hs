-- pointless.hs
module Pointfree where

f1 :: Int -> [Int] -> Int
f1 z xs = foldr (+) z xs


f2 :: Int -> [Int] -> Int
f2 = foldr (+)

f3 = length . filter (== 'a')
-- f3 "abcda" == 2
