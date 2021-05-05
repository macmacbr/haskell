module WarmUps where


-- 1. stops and vowels
stops :: [Char]
stops = "pbtdkg"
vowels :: [Char]
vowels = "aeiou"
createSVS :: [[Char]]
createSVS = 
  [ a : b : c : "" | a <- stops, b <- vowels, c <- stops]

onlyPSVS :: [[Char]]
onlyPSVS = [ a | a <- createSVS, head(a) == 'p']

nouns :: [[Char]]
nouns = ["pen","apple","pineapple","pin","pout"]
verbs :: [[Char]]
verbs = ["ingested", "poked", "slouched"]

createNVN :: [([Char], [Char], [Char])]
createNVN = [ (a, b, c) | a <- nouns, b <- verbs, c <- nouns]

