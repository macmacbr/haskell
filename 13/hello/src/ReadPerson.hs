module ReadPerson where

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid =
      NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson :: Name
    -> Age
    -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
          "Name was: " ++ show name ++
          " Age was: " ++ show age
          
gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Enter Person Name and Age"
  name <- getLine
  age  <- readLn 
  case mkPerson name age of
    Right _ -> putStrLn "Yay! Person created"
    Left p -> print p