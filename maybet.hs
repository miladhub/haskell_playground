module MyMaybeExercises where

import Control.Monad.Trans.Maybe

foo :: MaybeT IO String
foo = MaybeT $ return (Just "foo")

bar :: MaybeT IO String
bar = MaybeT $ return (Just "bar")

baz :: MaybeT IO String
baz = MaybeT $ return Nothing

comb :: MaybeT IO String -> MaybeT IO String -> IO (Maybe String)
comb f g = runMaybeT $ do
  mf <- f
  mg <- g
  return (mf ++ mg)

{--
*MyMaybeExercises> comb foo bar
Just "foobar"
*MyMaybeExercises> comb foo baz
Nothing
*MyMaybeExercises> 
--}

