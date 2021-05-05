module Lib
    ( someFunc,
      sumfr,
      lengthfr,
      productfr,
      concatfr,
      foldrfr,
      foldSilly
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sumfr :: [Integer] -> Integer
sumfr [] = 0
sumfr (x:xs) = x + sumfr xs

lengthfr :: [a] -> Integer
lengthfr [] = 0
lengthfr (x:xs) = 1 + lengthfr xs

productfr :: [Integer] -> Integer
productfr [] = 1
productfr (x:xs) = x * productfr xs

concatfr :: [[a]] -> [a]
concatfr [] = []
concatfr (x:xs) = x ++ concatfr xs

foldrfr :: (a -> b -> b) -> b -> [a] -> b
foldrfr f z [] = z
foldrfr f acc (x:xs) = f x (foldrfr f acc xs)

foldSilly :: [a] -> [Char]
foldSilly = foldr (\_ _ -> "Silly") "0"

