-- arith2.hs
module Arith2 where

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

main :: IO ()
main = do
  print (0 :: Int)
  print (add 1 0)
  print (addOne 0)
  print (addOnePF 0)
  print ((addOne . addOne) 0)
  print ((addOnePF . addOne) 0)
  print ((addOne . addOnePF) 0)
  print ((addOnePF . addOnePF) 0)
  print (negate (addOne 0))
  print ((negate . addOne) 0)
  print ((addOne . addOne . addOne . negate . addOne) 0)

print1 :: Show a => a -> IO()
print1 a = putStrLn (show a)

print2 :: Show a => a -> IO()
print2 a = (putStrLn . show) a

print3 :: Show a => a -> IO()
print3 = (.) putStrLn show

tensDigit :: Integral a => a -> a
tensDigit x = d
   where  d = snd ( divMod x 10 ) 

hundredsDigit :: Integral a => a -> a
hundredsDigit x = d
   where  d = snd $ divMod x 100 

foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True -> x
    False -> y

foldBool2:: a -> a -> Bool -> a
foldBool2 x y b 
  | b = x
  | otherwise = y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

g :: (a->b) -> (a,c) -> (b,c)
g f t = (f(fst t), snd t) 
