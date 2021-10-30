module Unfolds where

import Data.List

mehSum :: Num a => [a] -> a
mehSum = go 0
      where go :: Num a => a -> [a] -> a
            go n [] = n
            go n (x:xs') = go (n + x) xs'

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct = go 1
          where go :: Num a => a -> [a] -> a
                go n [] = n
                go n (x:xs') = go (n * x) xs'

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1


mehConcat :: [[a]] -> [a]
mehConcat = go []
        where go :: [a] -> [[a]] -> [a]
              go xs' [] = xs'
              go xs' (x:xs'') = go (xs' ++ x) xs''

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []     -- the code analyzer wants to use concat here. :)

-- page 480
-- 1. myIterate
myIterate :: (a -> a) -> a -> [a]
myIterate f = go
        where go a1 = a1 : go (f a1)

-- 2. unfoldr
myUnfoldr :: (b -> Maybe (a, b))
          -> b
          -> [a]
myUnfoldr fm = go . fm
        where  go Nothing = []
               go (Just (a1, b1)) = a1 : go (fm b1)

-- 3. better iterate
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))

-- Binary Trees
data BinaryTree a =
          Leaf
        | Node (BinaryTree a) a (BinaryTree a)
        deriving (Eq, Ord, Show)
-- 1.
btunfold :: (a -> Maybe (a, b, a))
          -> a
          -> BinaryTree b
btunfold growF = goGrowF
         where goGrowF = go . growF
               go Nothing = Leaf :: BinaryTree a
               go (Just (a1, b1, a2)) = Node (goGrowF a1) b1 (goGrowF a2)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = btunfold
                (\i -> if i >= 0 
                          then Just (i - 1, n - i, i - 1)
                          else Nothing
                 )
                n

