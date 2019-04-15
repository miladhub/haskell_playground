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
import Control.Monad.IO.Class

data Config = Config {
  counts :: IORef (M.Map Text Integer)
, prefix :: Text
}

type MyType =
  ReaderT Config IO
type Scotty =
  ScottyT Text MyType
type Handler =
  ActionT Text MyType

bumpBoomp :: Text
  -> M.Map Text Integer
  -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  let mv = M.lookup k m
  in case mv of
    (Just v) -> (m, v)
    Nothing -> (M.insert k 0 m, 0)

app :: Scotty ()
app =
  get "/:key" $ counter

counter :: ActionT Text MyType ()                                                                
counter = do
  unprefixed <- param "key"
  liftIO (putStrLn "hello")
  config <- lift ask
  let key' = mappend (prefix config) unprefixed
  newInteger <- lift $ readAndUpdateCount key' config
  html $
    mconcat [ "<h1>Success! Count was: "
            , TL.pack $ show newInteger
            , "</h1>"
            ]

readAndUpdateCount :: Text -> Config -> MyType Integer
readAndUpdateCount key config =
  let c = do
      cc <- readIORef $ counts config
      let (oldMap, oldCount) = bumpBoomp key cc
      let newMap = M.insert key (oldCount + 1) oldMap
      writeIORef (counts config) newMap
      return oldCount
  in liftIO c

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
  scottyT 3000 (runR config) app

runR :: Config -> MyType Response -> IO Response
runR config rt =
  let r = runReaderT rt
  in r config
