module Chap13 where

sayHello :: String -> IO () 
sayHello name = do
  putStrLn ("Hi " ++ name ++ "!")

dogs :: IO ()
dogs = do
  putStrLn "Who's a good puppy?!"
  putStrLn "YOU ARE!!!!!"

main :: IO ()
main = do
  name <- getLine
  sayHello name
  dogs

