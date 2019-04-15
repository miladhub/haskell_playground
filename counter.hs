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
bumpBoomp k m = undefined

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    let key' = mappend undefined unprefixed
    (newInteger :: Integer) <- undefined
    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack $ show newInteger
              , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter prefixArg
      runR = undefined
  scottyT 3000 runR app

runR :: ReaderT Config IO Response -> IO Response
runR = undefined
