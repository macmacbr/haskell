module Cypher where

import Data.Char

--Vigenere Cypher
---- from Ceaser #09

rotateChar :: Int -> Char -> Char
rotateChar offset c
       |  isAsciiLower c = chr (shiftit 'a')
       |  isAsciiUpper c = chr (shiftit 'A')
       |  otherwise = c
        where shiftit base = ord base + mod (offset + ord c - ord base) 26

caesar :: Int -> [Char] -> [Char]
caesar shift = map (rotateChar shift)

newtype CharKey = CharKey [Char] deriving (Eq, Show)
reverseCharKey :: CharKey -> CharKey
reverseCharKey (CharKey ck) = CharKey (reverse ck)

data RotCharRes = RotCharRes {
        theChars :: [Char]
      , theKey :: CharKey
      }  deriving (Eq, Show)

rotateCharKey :: Char -> RotCharRes -> RotCharRes
rotateCharKey c rcr
       |  isAsciiLower c = RotCharRes (theChars rcr ++ [chr (shiftIt 'a' (offs (theKey rcr)))]) (rotCk (theKey rcr))
       |  isAsciiUpper c = RotCharRes (theChars rcr ++ [chr (shiftIt 'A' (offs (theKey rcr)))]) (rotCk (theKey rcr))
       |  otherwise = RotCharRes (theChars rcr ++ [c]) (theKey rcr)
       where
         shiftIt base offset = ord base + mod (offset + ord c - ord base) 26
         rotCk kc =
           case kc of
             (CharKey (ck : cks)) -> CharKey $ cks ++ [ck]
             (CharKey []) -> CharKey []
         offs kc =
           case kc of
             (CharKey (ck : _)) -> if isAsciiLower ck then ord ck - ord 'a' else ord ck - ord 'A'
             (CharKey []) -> 0
