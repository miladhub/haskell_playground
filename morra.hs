{-# LANGUAGE ScopedTypeVariables #-}

module Morra where

import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import System.Random

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
  mw <- turn
  case mw of
    Nothing -> game
    Just w  -> return w

turn :: StateT Scores IO (Maybe Winner)
turn = StateT $ \s -> do
  w <- throw
  let newScore = scores s w
  if (p newScore) == 3 || (c newScore) == 3 then
    return (Just w, newScore)
  else
    return (Nothing, newScore)

throw :: IO Winner
throw = do
  putStr "P: "
  p <- readLn
  c <- oneOrTwo
  putStrLn $ "C: " ++ (show c)
  let winner = if (p + c) `mod` 2 == 0 then C else P
  putStrLn $ "- " ++ (show winner) ++ " wins"
  return winner

scores :: Scores -> Winner -> Scores
scores s P = Scores ( (p s) + 1 ) (c s)
scores s C = Scores (p s) ( (c s) + 1 )

oneOrTwo :: IO Integer
oneOrTwo = getStdRandom (randomR (1,2))
