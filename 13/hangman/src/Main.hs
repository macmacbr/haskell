module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)
import System.Random (randomRIO)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = 
         freshPuzzle (fmap toLower word)
  runGame puzzle

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

maxTries :: Int
maxTries = 15

newtype WordList = 
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w = 
          let l = length (w :: String)
          in      l >= minWordLength
               && l <  maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
   randomIndex <- randomRIO (0, (length wl) - 1)
   return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = 
  Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
   show (Puzzle _ discovered guessed) = 
      (intersperse ' ' $
       fmap renderPuzzleChar discovered)
       ++ ". Guessed so far (" ++ (show $ length guessed) ++ "): " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (\_ -> Nothing) word) ""

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) = flip elem word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) = flip elem guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledSoFar guessedSoFar) c =
 Puzzle word newFilled (c : guessedSoFar)
 where zipper newGuess wordChar filledChar =
          if wordChar == newGuess
            then Just wordChar
            else filledChar
       newFilled = 
         zipWith (zipper c) word filledSoFar 

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do 
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn $ "You guessed " ++ [guess] ++ " already, please pick another letter"
      return puzzle
    (True, _) -> do
      putStrLn "You found a new letter."
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "Not in the word. Try again"
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > maxTries then
    do putStrLn "You lose!"
       putStrLn $
          "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin p@(Puzzle _ filledIn _) =
  if all isJust filledIn then
    do putStrLn $ (show p)
       putStrLn "Nailed it!"
       exitSuccess
  else
    return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStrLn "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStr "Need to guess a single letter at a time"

 
