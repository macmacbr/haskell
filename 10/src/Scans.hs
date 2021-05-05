module Scans where

-- dangerous, infinite function
fibs :: [Integer] 
fibs = 1 : scanl (+) 1 fibs

fibsN ::  Int -> Integer
fibsN x = fibs !! x

fibs20First :: [Integer]
fibs20First = take 20 fibs

fibsLessThan100 :: [Integer]
fibsLessThan100 = takeWhile (<100) fibs

factorial :: [Integer]
factorial = scanl (\a b -> a * (b+1)) 1 [1..]

factorial1 :: [Integer]
factorial1 = scanl (flip $ (*) . succ) 1 [1..]