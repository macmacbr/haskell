module PhoneText where

type Digit = Char
  
-- data DaPhone1 = DaPhone1 {
--   keys :: [Key]
-- }

newtype DaPhone = DaPhone [Key]

data Key = Key {
     digit :: Digit
   , symbols :: String
} deriving (Eq, Show)

key1 = Key {digit = '1', symbols = "1"}
key2 = Key {digit = '2', symbols = "ABC2"} 
key3 = Key {digit = '3', symbols = "DEF3"} 
key4 = Key {digit = '4', symbols = "GHI4"} 
key5 = Key {digit = '5', symbols = "JKL5"} 
key6 = Key {digit = '6', symbols = "MNO6"} 
key7 = Key {digit = '7', symbols = "PQRS7"} 
key8 = Key {digit = '8', symbols = "TUV8"} 
key9 = Key {digit = '9', symbols = "WXYZ9"} 
keyStar = Key {digit = '*', symbols = "^*"}  {-  ^ means uppercase -}
key0 = Key {digit = '0', symbols = "+ _0"}   {- space bar included -} 
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

findTextKey :: DaPhone
        -> Char
        -> Maybe Key
findTextKey (DaPhone []) _ = Nothing
findTextKey (DaPhone (k : ks)) c = 
   if findChar (symbols k) c then Just k else findTextKey (DaPhone ks) c



{-
reverseTaps :: DaPhone
             -> Char
             -> [(Digit, Presses)]
reverseTaps dp c = findDigits (isUpperCase c) (upperCase c)
        where findDigits True uc = (keyStar Digit, Presses 1) : findDigits False uc
              findDigits False uc =
-}

