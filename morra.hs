{-# LANGUAGE ScopedTypeVariables #-}

module Morra where

import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import System.Random

data Winner =
  Player | Computer
  deriving (Eq, Show)

data Scores =
  Scores {
    player   :: Integer
  , computer :: Integer
  }
  deriving (Eq, Show)

type Game a = StateT Scores IO a
type Turn a = StateT Scores IO (Maybe a)

main :: IO ()
main = do
  putStrLn "-- P is Player"
  putStrLn "-- C is Computer"
  putStrLn "-- Player is odds, computer is evens."
  winner <- run
  putStrLn $ "Contratulations to " ++ (show winner) ++ "!"

run :: IO Winner
run = do
  (winner, _) <- runStateT game (Scores 0 0)
  return winner

game :: Game Winner
game = do
  mw <- turn
  case mw of
    Nothing -> game
    Just w  -> return w

turn :: Turn Winner
turn = StateT $ \s -> do
  w <- throw
  let newScore = scores s w
  if (player newScore) == 3 || (computer newScore) == 3 then
    return (Just w, newScore)
  else
    return (Nothing, newScore)

throw :: IO Winner
throw = do
  putStr "P: "
  p <- readLn
  c <- oneOrTwo
  putStrLn $ "C: " ++ (show c)
  let winner = if (p + c) `mod` 2 == 0 then Computer else Player
  putStrLn $ "- " ++ (show winner) ++ " wins"
  return winner

scores :: Scores -> Winner -> Scores
scores s Player   = Scores ( (player s) + 1 ) (computer s)
scores s Computer = Scores (player s) ( (computer s) + 1 )

oneOrTwo :: IO Integer
oneOrTwo = getStdRandom (randomR (1, 2))
