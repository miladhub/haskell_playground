{-# LANGUAGE ScopedTypeVariables #-}

module Morra where

import Control.Monad
import Control.Monad.Trans.State.Lazy

data Winner =
  P | C
  deriving (Eq, Show)

data Scores =
  Scores {
    p :: Integer,
    c :: Integer
  }
  deriving (Eq, Show)

main :: IO ()
main = do
  putStrLn "-- P is Player"
  putStrLn "-- C is Computer"
  putStrLn "-- Player is odds, computer is evens."
  (_, s) <- runStateT game $ Scores 0 0
  putStrLn $ "Final score " ++ (show s)

game :: StateT Scores IO ()
game = replicateM_ 3 $ StateT $ \s -> do
  w <- play
  return ((), scores s w)

play :: IO Winner
play = do
  putStr "P: "
  p <- readLn
  putStr "C: "
  c <- readLn
  let winner = if (p + c) `mod` 2 == 0 then C else P
  putStrLn $ "- " ++ (show winner) ++ " wins"
  return winner

scores :: Scores -> Winner -> Scores
scores s P = Scores ( (p s) + 1 ) (c s)
scores s C = Scores (p s) ( (c s) + 1 )

