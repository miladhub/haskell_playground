module People (Person(..), findPerson) where

import Chap26

data Person =
  Person {
    name :: String,
    age :: Integer
  }
  deriving (Eq, Show)

findPerson :: String -> IO (Maybe Person)
findPerson name = do
  f <- readFile "people.txt"
  return (matchPerson (lines f) name)  

matchPerson :: [String] -> String -> Maybe Person
matchPerson rows byName =
  let people = toPeople rows
      matches = filter (((==) byName) . name) people
  in if length matches > 0 then
    Just (head matches)
  else
    Nothing

toPeople :: [String] -> [Person]
toPeople rows =
  let split = fmap words rows
  in fmap toPerson split where
    toPerson s =
      let n = s!!0
          a = read $ s!!1
      in Person n a
