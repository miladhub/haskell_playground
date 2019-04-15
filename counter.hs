{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Trans.Class 
import Control.Monad.Trans.Reader 
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL 
import System.Environment (getArgs) 
import Web.Scotty.Trans
import Network.Wai

data Config = Config {
  counts :: IORef (M.Map Text Integer)
, prefix :: Text
}

type Scotty =
  ScottyT Text (ReaderT Config IO)
type Handler =
  ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
  -> M.Map Text Integer
  -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  let mv = M.lookup k m
  in case mv of
    (Just v) -> (m, v)
    Nothing -> (M.insert k 0 m, 0)

--app :: Scotty ()
app :: ScottyT Text (ReaderT Config IO) ()
app =
  get "/:key" $ counter

counter :: ActionT Text (ReaderT Config IO) ()                                                                
counter = do
  unprefixed <- param "key"
  config <- lift ask
  let key' = mappend (prefix config) unprefixed
  (newInteger :: Integer) <- return 42
  html $
    mconcat [ "<h1>Success! Count was: "
            , TL.pack $ show newInteger
            , "</h1>"
            ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
  scottyT 3000 (runR config) app

runR :: Config -> ReaderT Config IO Response -> IO Response
runR config rt =
  let r = runReaderT rt
  in r config
