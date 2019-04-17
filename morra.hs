{-# LANGUAGE ScopedTypeVariables #-}

module Morra where

import Control.Monad
import Control.Monad.Trans.State.Lazy

main :: IO ()
main = forever loop

loop = do
  putStrLn "-- p is player"
  putStrLn "-- c is computer"
  putStr "P: "
  (p :: Integer) <- readLn
  putStr "C: "
  (c :: Integer) <- readLn
  putStrLn $ "Sum: " ++ (show $ p + c)

data Scores =
  Scores {
    p :: Integer,
    c :: Integer
  }
  deriving (Eq, Show)

foo :: StateT Scores IO ()
foo = StateT $ \s -> do
  return ((), s)
