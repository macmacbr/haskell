-- matchingTuples1.hs
module TupleFunctions where

--These have to be the same type, because (+) has the type a -> a -> a:
addEmUp2    :: Num a => (a, a) -> a
addEmUp2    (x, y) = x + y

-- addEmUp2 could also be written like so:
addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a,b,c) -> b
snd3 (_, x, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

