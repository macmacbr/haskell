module Main where

-- import Hello
-- import DogsRule
import Cypher
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Please enter encryption key and text to ceaser and vigenere encrypt/decrypt" 
  key <- getLine
  text <- getLine
  putStrLn $ "ceaser 13:" ++ caesar 13 text
  putStrLn $ "vigenere:" ++  vigenere (CharKey key) text


