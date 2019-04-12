module Transf where

import Chap26

readMoreThan42 :: IO (Maybe Integer)
readMoreThan42 =
  do
    s <- readLn
    if (s > 42) then
      return $ Just s
    else
      return Nothing

readAndSum :: Integer -> Either String (IO (Maybe Integer))
readAndSum i =
  if (i < 10) then
    Left "must be >= 10"
  else
    Right $ do
      mj <- readMoreThan42
      case mj of
        Just j -> return $ Just (i + j)
        Nothing -> return Nothing

run :: Integer -> IO ()
run i =
  case readAndSum i of
    Left e -> putStrLn e
    Right iomi -> do
      mi <- iomi
      case mi of
        Just j -> putStrLn (show j)
        Nothing -> putStrLn "sorry"

