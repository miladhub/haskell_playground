module Morra where

import Control.Monad

main :: IO ()
main = loop

loop = forever $ do
  putStrLn "-- p is player"
  putStrLn "-- c is computer"
  putStr   "P: "
  p <- readLn
  putStr   "C: "
  c <- readLn
