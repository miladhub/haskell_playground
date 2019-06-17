{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module MyMonadState where

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.State.Lazy (StateT)
import Control.Monad.Trans.Class (lift)

class Monad m => MonadState s m where
  get :: m s
  get = state (\s -> (s, s))

  put :: s -> m ()
  put s = state (\_ -> ((), s))

  state :: (s -> (a, s)) -> m a
  state f = do
    s <- get
    let (a, s') = f s
    put s'
    return a

instance MonadState s m => MonadState s (MaybeT m) where
  get = lift get      
  put = lift . put
  state = lift . state

instance MonadState s m => MonadState s (StateT s m) where
  get = undefined
  put = undefined
  state = undefined
