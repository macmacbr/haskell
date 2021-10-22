{-# LANGUAGE TupleSections #-}

module PhoneText where

import Data.Char (isUpper, toUpper)


type Digit = Char
  
-- data DaPhone1 = DaPhone1 {
--   keys :: [Key]
-- }

newtype DaPhone = DaPhone [Key]

data Key = Key {
     digit :: Digit
   , symbols :: String
} deriving (Eq, Show)

key1 :: Key
key1 = Key {digit = '1', symbols = "1"}
key2 :: Key
key2 = Key {digit = '2', symbols = "ABC2"} 
key3 :: Key
key3 = Key {digit = '3', symbols = "DEF3"} 
key4 :: Key
key4 = Key {digit = '4', symbols = "GHI4"} 
key5 :: Key
key5 = Key {digit = '5', symbols = "JKL5"} 
key6 :: Key
key6 = Key {digit = '6', symbols = "MNO6"} 
key7 :: Key
key7 = Key {digit = '7', symbols = "PQRS7"} 
key8 :: Key
key8 = Key {digit = '8', symbols = "TUV8"} 
key9 :: Key
key9 = Key {digit = '9', symbols = "WXYZ9"} 
keyStar :: Key
keyStar = Key {digit = '*', symbols = "^*"}  {-  ^ means uppercase -}
key0 :: Key
key0 = Key {digit = '0', symbols = "+ _0"}   {- space bar included -} 
keyFlat :: Key
keyFlat = Key {digit = '#', symbols = ".,#"}

myPhone :: DaPhone
myPhone = DaPhone [key1, key2, key3, key4, key5, key6, key7, key8, key9, keyStar, key0, keyFlat]


convo :: [String]
convo = ["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol OK. Have u ever tasted alcohol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "OK. Do u think I am pretty Lol",
        "Lol ya",
        "Just making sure rofl ur turn"]
        
type Presses = Int


findChar :: String -> Char -> Bool
findChar [] _ = False
findChar (s : xs) c = (s == c) || findChar xs c

charIndex :: String -> Char -> Int
charIndex str c = head [ ct | (sc, ct) <- zip str [1..], sc == c]

findTextKey :: DaPhone
        -> Char
        -> Maybe Key
findTextKey (DaPhone []) _ = Nothing
findTextKey (DaPhone (k : ks)) c = 
   if findChar (symbols k) c then Just k else findTextKey (DaPhone ks) c


findDigitAndPresses :: DaPhone
         -> Char
         -> [(Digit, Presses)]
findDigitAndPresses dp c =
   maybeToSeq $ findTextKey dp c
     where maybeToSeq (Just k) = [(digit k, charIndex (symbols k) c)]
           maybeToSeq Nothing = []


reverseTaps :: DaPhone
         -> Char
         -> [(Digit, Presses)]
reverseTaps dp c = findDigits (isUpper c) (toUpper c)
        where findDigits True uc = (digit keyStar, 1) : findDigits False uc
              findDigits False uc = findDigitAndPresses dp uc 


cellPhonesDead :: DaPhone
         -> String
         -> [(Digit, Presses)]
cellPhonesDead dp = concatMap (reverseTaps dp)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, p) a -> a + p ) 0 

countSingleLetter :: Char -> String -> (Char, Int)
countSingleLetter c str = (c, length(filter(\a -> toUpper a == c) str))

countLetters :: String -> [(Char, Int)]
countLetters text = map (`countSingleLetter` text) ['A'..'Z']

getMax :: [(a, Int)] -> Maybe a
getMax [] = Nothing
getMax ((a1, i) : as1) = gs a1 i as1 
    where gs c _ [] = Just c
          gs c m ((aa,n ): as) = if n > m then gs aa n as else gs c m as 

mostPopularLetter :: String -> Maybe Char
mostPopularLetter = getMax . countLetters