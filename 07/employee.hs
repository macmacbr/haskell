{-# LANGUAGE NoMonomorphismRestriction #-}
-- employee.hs
module Employee where

data Employee = 
                Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO()
reportBoss e e' =
  putStrLn $ show e ++
             " is the lord of " ++
             show e'

employeeRank :: Employee -> Employee -> IO()
employeeRank e e' =
  case compare e e' of
    GT -> reportBoss e e'
    LT -> (flip reportBoss) e e'
    EQ -> putStrLn "Found some\
                   \ co-workers"


employeeRank2 :: (Employee 
                   -> Employee 
                   -> Ordering) 
                  -> Employee 
                  -> Employee 
                  -> IO()
employeeRank2 sf e e' =
  case sf e e' of
    GT -> reportBoss e e'
    LT -> (flip reportBoss) e e'
    EQ -> putStrLn "Found some\
                   \ co-workers"


codersRuleCEOsDrool :: Employee
                      -> Employee
                      -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

dodgy :: Integer -> Integer -> Integer
dodgy x y = x + y * 10
oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1
oneIsTwo :: Integer -> Integer
oneIsTwo = (flip dodgy) 2

myAbs :: Integer -> Integer
myAbs i
  | i < 0  = (-i)
  | otherwise = i

bloodNa :: Integer -> String
bloodNa x
  | x < 135 = "too low"
  | x > 145 = "too high"
  | otherwise = "just right"

-- c is the hypotenuse of
-- the triangle.
isRight :: (Num a, Eq a)
        => a -> a -> a -> String
isRight a b c
  | a^2 + b^2 == c^2 = "RIGHT ON"
  | otherwise = "not right"

dogYrs :: Integer -> Integer
dogYrs x
  | x <= 0 = 0
  | x <= 1 = x * 15
  | x <= 2 = x * 12
  | x <= 4 = x * 8
  | otherwise = x * 6

avgGrade :: (Fractional a, Ord a)
              => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100



rt3 :: (Show a, Read b) => a -> b
rt3 p = read . show $ p

main = do
  print (rt3 4)

