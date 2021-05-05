-- factorial.hs
module Factorial where

factorial :: Integer -> Integer
factorial n 
  | n == 0 = 1
  | (n > 0 ) = n * factorial (n - 1)
  | otherwise = error "Negative factorial"


inc :: Num a => a -> a
inc = (+1)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = 
  n
incTimes x n =
  1 + (incTimes (x-1) n)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = f . applyTimes (n-1) f $ b

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' x n = applyTimes x (+1) n

fibonacci :: Integral a => a -> a
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = (+) (fibonacci (n-1)) (fibonacci (n-2))

type DivNum = Integer
type DivDen = Integer
type DivQuo = Integer
type DivRem = Integer

dividedBy :: DivNum -> DivDen -> (DivQuo, DivRem)
dividedBy num den = go num den 0
  where go n d count
          | n < d = (count, n)
          | otherwise = 
              go (n-d) d (count + 1)

sumRec :: (Eq a, Num a) => a -> a
sumRec a = go a 0
  where go x ac
         | x == 0 = ac 
         | otherwise = go (x-1) (ac+x) 

mult2 :: (Integral a) => a -> a -> a
mult2 a b = go a b 0
  where go x y ac
         | (x == 0 || y == 0) = ac
         | otherwise = go (x-1) y (ac + y) 


sumRec3 :: (Integral a) => a -> a -> a
sumRec3 a b = go a b 0
  where go x y ac
         | x > 0 && y > 0 = go (x-1) (y-1) (ac+x+y) 
         | x == 0 = ac + sumRec y
         | y == 0 = ac + sumRec x
         | otherwise = ac  -- *error*

data DivResult =
    DividedResult (DivQuo, DivRem)
  | DividedByZero
  deriving Show


dividedBy2 :: DivNum -> DivDen -> DivResult
dividedBy2 num den = go (abs(num)) (abs(den)) (0,  (num * den >= 0))
  where go n d (count, sig)
          | d == 0 = DividedByZero
          | n < d = DividedResult (count * (if sig then 1 else (-1)), n)
          | otherwise = 
              go (n-d) d (count + 1, sig)

mc91 :: (Integral a) => a -> a 
mc91 a 
  | a > 100 = a - 10
  | otherwise = (mc91 . mc91) (a + 11)

