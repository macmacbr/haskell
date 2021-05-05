--module Ciphers.hs
module Ciphers where

import Data.Char

rotateChar :: Int -> Char -> Char 
rotateChar offset c 
       |  c >= 'a' && c <= 'z' = chr (shiftit 'a')
       |  c >= 'A' && c <= 'Z' = chr (shiftit 'A') 
       |  otherwise = c
        where shiftit base = ord base + mod (offset + ord c - ord base) 26

caesar :: Int -> [Char] -> [Char] 
caesar shift fromText = map (rotateChar shift) fromText

uncaesar :: Int -> [Char] -> [Char] 
uncaesar shift fromCipher = map (rotateChar ((-1)*shift)) fromCipher


myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs)
       | x == False = False
       | otherwise = myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
   | x == True = True
   | otherwise = myOr xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) 
   | (f x) == True = True
   | otherwise = myAny f xs

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 _ [] = False
myElem1 e (x:xs) = 
  if (e == x) then True
              else myElem1 e xs

myElem :: Eq a => a -> [a] -> Bool
myElem el xs = myAny (== el) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:[]) = [x]
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (lh:[]) = lh
squish (lh:ls) = lh ++ squish ls


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (a:as) = (f a) ++ squishMap f as


squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myComparerBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myComparerBy _ _ [] = undefined
myComparerBy o f (a:as) = go f as a
      where 
         go _ [] w = w
         go cm (x:xs) w = if (cm x w == o) 
                              then go cm xs x 
                              else go cm xs w

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = myComparerBy GT

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myComparerBy LT 

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare



