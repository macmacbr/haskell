module Exercises where

-- pg 380+


-- 1. myOr with folding
myOr :: [Bool] -> Bool
myOr = foldr (||) False
 
-- 2. myAny with folding
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr(\a b -> (||) b (f a)) False

-- 3. myElem with using any and folding
myElem :: Eq a => a -> [a] -> Bool
myElem el = myAny (== el)

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 el = foldr(\a b -> (||) b (a==el)) False

-- 4. reverse
myReverse :: [a] -> [a]
myReverse = foldl (flip(:)) []

-- 5. myMap
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f(a) : b) []

-- 6. myFilter (pointless)
myValid :: (a -> Bool) -> a -> [a] -> [a]
myValid f a xs = if f a then (:) a xs else xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter = flip foldr [] . myValid

-- 7. squish
squish :: [[a]] -> [a]
squish = foldr (++) []
                   
-- 8. squishMap
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) [] 

squishMapPointless :: (a -> [b]) -> [a] -> [b]
squishMapPointless = flip foldr [] . squisher
         where squisher f = \a b -> f a ++ b 

squishMap1 :: (a -> [b]) -> [a] -> [b]
squishMap1 f = squish . myMap f

-- 9. squishAgain using squishMap
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10. myMaximumBy 
myWinner :: Ordering -> (a -> a -> Ordering) -> a -> a -> a
myWinner o f a1 a2 = if f a1 a2 == o then a1 else a2

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldl (myWinner GT f) (head xs) (tail xs)  

-- 11. myMinimumBy
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldl (myWinner LT f) (head xs) (tail xs)  