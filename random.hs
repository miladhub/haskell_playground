module MyRandom where

import Control.Applicative (liftA3) 
import Control.Monad (replicateM) 
import Control.Monad.State 
import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die 
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree 
    4 -> DieFour 
    5 -> DieFive 
    6 -> DieSix
    -- Use 'error'
    -- _extremely_ sparingly. 
    x ->
      error $
        "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die) 
rollDieThreeTimes = do
  let
    s = mkStdGen 0
    (d1, s1) = randomR (1, 6) s 
    (d2, s2) = randomR (1, 6) s1 
    (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die 
rollDie = state $ do
  (n, s) <- randomR (1, 6) 
  return (intToDie n, s)

{- using monad of functions here; same as:
rollDie = state $ \olds ->
  let (n, s) = randomR (1, 6) olds 
  in (intToDie n, s)
-}

rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 6))


rollDieThreeTimes' :: State StdGen (Die, Die, Die) 
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie
