-- greetIfCool3.hs
module GreetIfCool3 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True ->
      putStrLn "eyyyyy. What's shakin'?"
    False ->
      putStrLn "pshhhh."
  where cool =
          coolness == "downright frosty yo"

functionC :: Ord a => a -> a -> a 
functionC x y =
  case isdecrescent of
    True ->
      x
    False ->
      y 
  where isdecrescent = x > y

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n =
  case isEven of
    True ->
      n+2
    False ->
      n
  where isEven = even n

nums :: Integer -> Integer
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

myFlip :: ( a -> a -> c) -> (a -> a -> c)
myFlip f a b = f b a 

myFlip2 :: ( a -> a -> c) -> (a -> a -> c)
myFlip2 f = \ a b -> f b a 

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnAfterApply :: (a -> b) -> a -> c -> b
rvgGrade :: (Fractional a, Ord a)
=> a -> Char
avgGrade x
| y >= 0.9 = 'A'
| y >= 0.8 = 'B'
| y >= 0.7 = 'C'
| y >= 0.59 = 'D'
| y < 0.59 = 'F'
where y = x / 100eturnAfterApply f a c = f a
