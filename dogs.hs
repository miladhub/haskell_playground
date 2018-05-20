sayHello :: String -> IO ()
sayHello name =
  putStrLn ("Hi " ++ name ++ "!")

main :: IO ()
main = do
  name <- getLine
  sayHello name
