module UnderstandingFolds where

-- Exercise 1
-- foldr (*) 1 [1..5] is the same as
-- b and c

-- ex 2
flipInt :: Int -> Int -> Int
flipInt = flip (*)

x1 = foldl flipInt 1 [1..3]
x2a = ((1 `flipInt` 1) `flipInt` 2) `flipInt` 3  
x2b = (1 `flipInt` 2) `flipInt` 3
x2c = 2 `flipInt` 3
x2d = 6 :: Int

-- ex 3
--   c- foldr, but not foldl, associates to the right.

-- ex 4
-- a - reduce structures

-- ex 5
fiveA = foldr (++) [] ["woot", "WOOT", "woot"]
fiveB = foldr max ' ' "fear is the little death"
fiveC = foldr (&&) True [False, True]
-- This one is more subtle than the previous. Can it ever return
   --a different answer?
fiveD = foldr (||) False [False, True]
fiveE = foldl (flip((++) . show)) "" [1..5]
fiveF = foldr const 0 [1..5]
fiveG = foldr const ' ' "tacos"
fiveH = foldl (flip const) ' ' "burritos"
fiveI = foldl (flip const) 1 [1..5]