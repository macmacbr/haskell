module Chapter12 where

import Data.Char

  
type Name = String
type Age = Integer
data Person = Person Name Age deriving (Show, Eq)

mkPersonOne :: Name -> Age -> Maybe Person
mkPersonOne name age
  | name /= "" && age >= 0 =
      Just $ Person name age
  | otherwise = Nothing

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

mkPersonTwo :: Name
         -> Age
         -> Either PersonInvalid Person
mkPersonTwo name age
  | name /= "" && age >=0 =
        Right $ Person name age
  | name == "" =
        Left NameEmpty
  | otherwise =
        Left AgeTooLow

type ValidatePerson a = 
      Either [PersonInvalid] a
     
ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = if age >= 0 
                then Right age
                else Left [AgeTooLow]
  
nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = if name /= ""
                  then Right name
                  else Left [NameEmpty]
  
mkPerson :: Name -> Age -> Either [PersonInvalid] Person
mkPerson name age = 
  mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right name) (Right age) = Right $ Person name age
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge

-- exercises page 473
-- kind of a in id :: a -> a
--     a -> *
-- kind of a and f in r :: a -> f a
--     a = *
--     f = * -> *

-- 1. (pg 473)
lc :: String -> String
lc = map toLower

-- detects `the`
notThe :: String -> Maybe String
notThe word
  | lc word == "the" = Nothing
  | otherwise = Just word
  
strTokenizer :: String -> [String]
strTokenizer = foldr f [""]
    where f :: Char -> [String] -> [String]
          f _ [] = [""]
          f c textTokens@(newWord:textSoFar) 
            | isPunctuation c || isControl c || isSeparator c || isSpace c = 
                  if newWord == "" 
                      then "" : [c] : textSoFar
                      else "" : [c] : textTokens
            | otherwise = (c:newWord):textSoFar

replaceThe :: String -> String
replaceThe text = foldr (replaceToA . notThe) "" (strTokenizer text)
    where replaceToA :: Maybe String -> String -> String
          replaceToA Nothing acc = "a" ++ acc
          replaceToA (Just word) acc = word ++ acc
          
-- 2. (page 473)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel text = countTheBeforeVowel' $ map notThe $ filter isWord (strTokenizer text)

firstLetterIs :: (Char -> Bool) -> String -> Bool
firstLetterIs _ "" = False
firstLetterIs isC (c:_) = isC $ toLower c

firstLetterIsVowel :: String -> Bool
firstLetterIsVowel = firstLetterIs isVowel

isVowel :: Char -> Bool
isVowel = flip elem "aeiou"

isConsonant :: Char -> Bool
isConsonant c = isAlphaNum c && not (isVowel c)

isWord :: String -> Bool
isWord = firstLetterIs isAlphaNum

countTheBeforeVowel' :: [Maybe String] -> Integer
countTheBeforeVowel' ms = foldr f 0 $ zip ms (Just "" : ms)
        where 
          f (Just _, _) cn = cn
          f (_, Nothing) cn = cn
          f (Nothing, Just nextWord) cn = if firstLetterIsVowel nextWord then cn + 1 else cn 

-- 3. (page 474)
countElements :: (Char -> Bool) -> String -> Integer
countElements test tx = toInteger $ length $ filter test tx

countVowels :: String -> Integer
countVowels = countElements isVowel

countConsonants :: String -> Integer
countConsonants = countElements isConsonant

countVowelsTheHardWay :: String -> Integer
countVowelsTheHardWay = foldr f 0
    where f :: Char -> Integer -> Integer
          f c ct 
           | isVowel c = ct + 1
           | otherwise = ct
           
-- if the number of consonants is less than vowels, the word is invalid
newtype Word' = 
  Word' String
  deriving (Eq, Show)
  
validWord :: String -> Maybe Word'
validWord tx = if countVowels tx > countConsonants tx then Nothing else Just (Word' tx)
