{-# LANGUAGE ScopedTypeVariables #-}

module Morra where

import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class

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
  (w, s) <- runStateT game $ Scores 0 0
  putStrLn $ "Final score " ++ (show s)
  putStrLn $ "Winner " ++ (show w)

game :: StateT Scores IO Winner
game = do
  mw <- loop
  case mw of
    Nothing -> game
    Just w  -> return w

loop :: StateT Scores IO (Maybe Winner)
loop = StateT $ \s -> do
  w <- play
  let newScore = scores s w
  if (p newScore) == 3 || (c newScore) == 3 then
    return (Just w, newScore)
  else
    return (Nothing, newScore)

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
