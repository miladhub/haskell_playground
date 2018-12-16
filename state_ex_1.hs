module StateEx1 where

import Control.Monad
import Control.Monad.State
import Data.Functor.Identity

{-
foo :: String -> StateT [String] Maybe Int
foo s = do
  xs <- get
  put (s : xs)
  return $ length s
-}

get_ :: State s s
get_ = StateT $ \s -> return (s,s)

put_ :: s -> State s ()
put_ s = StateT $ \ss -> return ((), s)

exec :: State s a -> s -> s
exec (StateT sa) s =
  let ias = sa s
      as = runIdentity ias
  in snd as

eval :: State s a -> s -> a
eval (StateT sa) s = fst $ runIdentity (sa s)

modify_ :: (s -> s) -> State s ()
modify_ f = state $ \s -> ((), f s)
