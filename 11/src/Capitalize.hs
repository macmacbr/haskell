module Capitalize where

import Data.Char

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (' ':ts) = (:) ' ' (capitalizeWord ts)
capitalizeWord (t: ts) = (:) (toUpper t) ts

tupleCapitalizeWord :: String -> (String, String)
tupleCapitalizeWord s = (s, capitalizeWord s)

splitator :: (Char -> String -> String)
           -> Char 
           -> String 
           -> [String]
splitator agg sep = splitter []
   where splitter [] [] = []
         splitter t [] = [t]
         splitter t (w:ws) = if w == sep 
                                 then agg sep t : splitter [] ws 
                                 else splitter (t ++ [w])  ws 

keeper :: Char -> String -> String
keeper sep st = st ++ [sep]

dropper :: a -> String -> String
dropper _ st = st  

-- #449.2 Capitalize each word on a sentence and return as a tuple
capitalizeWords :: String -> [(String, String)]
capitalizeWords text = map tupleCapitalizeWord (splitator dropper ' ' text)

-- #450. 3. Capitalize paragraphs. Look out for spaces.
capitalizeParagraph :: String -> String
capitalizeParagraph text = concatMap capitalizeWord (splitator keeper '.' text) 
