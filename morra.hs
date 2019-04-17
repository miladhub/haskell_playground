{-# LANGUAGE ScopedTypeVariables #-}

module Morra where

import Control.Monad

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
