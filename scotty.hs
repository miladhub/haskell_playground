{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Web.Scotty
import Data.Monoid (mconcat)
import Web.Scotty.Internal.Types (ActionT(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy hiding (get)
import Control.Monad.IO.Class

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

liftStateT :: Monad m => m a -> StateT s m a
liftStateT m = StateT $ \s -> do
  a <- m
  return (a, s)

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    --lift (putStrLn "hello")
{-    (ActionT
      . (ExceptT . liftM Right)
      . liftReaderT
      . liftStateT
      ) (putStrLn "hello")
-}
    liftIO (putStrLn "hello")
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

