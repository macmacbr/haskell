{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheType where
import Data.List (sort)
-- simple example

myX = 1 :: Int
sigmund' :: Int -> Int 
sigmund' x = myX

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f1 a' b' = 
  (==) (f1 a') b'

f :: Num b => a -> b
f a = 2

jung :: [Char] -> Char
--jung :: Ord a => [a] -> a
jung xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

