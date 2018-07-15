module PersonMod where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
  NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
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
  putStrLn "Tell me the name:"
  name <- getLine
  putStrLn "Tell me the age:"
  age <- getLine
  let p = mkPerson name $ read age
  putStrLn $ case p of
    Right pp -> "Yay! Successfully got a person:" ++ show pp
    Left err -> show err

