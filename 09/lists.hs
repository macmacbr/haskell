--module lists
module Lists where

myTail :: [a] -> Maybe [a]
myTail []       = Nothing
myTail (_:[])   = Nothing
myTail (_ : xs) = Just xs


myHead :: [a] -> Maybe a
myHead []       = Nothing
myHead (h: _)   = Just h

-- enumFromToBool
eftBool :: Bool -> Bool -> [Bool]
eftBool a b 
  | a == b = [a]
  | otherwise  = [a, b]

-- enumFromTo with Ordering
eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b = go a b []
  where
    app x xs = xs ++ [x]
    go x y acc
      | x == y = app x acc
      | otherwise = go (succ x) y (app x acc)

-- enumFromTo with Comparison
eftEq :: (Ord e, Enum e) => e -> e -> [e]
eftEq a b = go a b []
  where
    app x xs = xs ++ [x]
    go x y acc
      | x > y = acc
      | otherwise = go (succ x) y (app x acc)

-- enumFromTo Int
eftInt2 :: Int -> Int -> [Int]
eftInt2 i j = eftEq i j

-- enumFromToChar
eftChar :: Char -> Char -> [Char]
eftChar i j = eftEq i j

-- enumFromTo old Int
eftInt :: Int -> Int -> [Int]
eftInt a b = go a b []
  where
    app x xs = xs ++ [x]
    go x y acc
      | x == y = app x acc
      | otherwise = go (succ x) y (app x acc)

-- fearful symmetry
--1. 
-- 
ns :: Char -> Char -> Bool
ns c =  (/= c)

fnw :: Char -> [Char] -> [Char]
fnw c = (dropWhile (not . (ns c))) . (dropWhile (ns c))

-- tokenizer by Char brkr.
breaker :: Char -> [Char] -> [[Char]]
breaker brkr phrase = 
  go phrase 
    where 
      go words
        | words == [] = []
        | otherwise =  (:) (takeWhile (ns brkr) words) $ go (fnw brkr words)

myWords phrase = breaker ' ' phrase

--2. & 3.
--
myLines :: String -> [String]
myLines text = breaker '\n' text


firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
             \ symmetry?"
sentences = firstSen ++ secondSen
          ++ thirdSen ++ fourthSen

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]


main :: IO ()
main =
  print $
    "Are they equal? "
      ++ show (myLines sentences
      == shouldEqual)

acro :: String -> String
acro xs = 
  [ x | x <- xs,
    elem x ['A'..'Z']
  ]


mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
--1. p 317 
tuples :: [(Int, Int)]
tuples = [
    (x, y) | 
        x <- mySqr,
        y <- myCube,
        x < 50, y < 50
  ]

-- zip List with combining function
zipWithMe :: (a -> b -> c) 
      -> [a] -> [b] -> [c]
zipWithMe _ [] _ = []
zipWithMe _ _ [] = []
zipWithMe fabc (a:as) (b:bs) = (fabc a b) : (zipWithMe fabc as bs)

-- zip list into tupples
zipMe :: [a] -> [b] -> [(a, b)]
zipMe = zipWithMe (\a -> \b -> (a, b))


