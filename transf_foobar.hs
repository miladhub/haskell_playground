module TransfFooBar where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

main :: IO ()
main = do
  mr <- runMaybeT $ run 1
  case mr of
    Nothing -> putStrLn "something went wrong"
    Just r  -> putStrLn r

run :: Int -> MaybeT IO String
run t = do
  f <- foo t
  b <- bar t
  return $ "foo: " ++ f ++ ", bar: " ++ b

foo :: Int -> MaybeT IO String
foo t = MaybeT $ do
  f <- getLine
  if (length f) > t then
    return $ Just f
  else
    do
      liftIO $ putStrLn "wrong foo"
      return Nothing

bar :: Int -> MaybeT IO String
bar t = MaybeT $ do
  f <- getLine
  if (length f) > t then
    return $ Just f
  else
    do
      liftIO $ putStrLn "wrong bar"
      return Nothing

main' :: IO ()
main' = do
  mr <- runMaybeT $ runReaderT run' 1
  case mr of
    Nothing -> putStrLn "something went wrong"
    Just r  -> putStrLn r

run' :: ReaderT Int (MaybeT IO) String
run' = do
  f <- foo'
  b <- bar'
  return $ "foo: " ++ f ++ ", bar: " ++ b

foo' :: ReaderT Int (MaybeT IO) String
foo' = do
  t <- ask
  f <- liftIO getLine
  return f
  if (length f) > t then
    return f
  else
    do
      liftIO $ putStrLn "wrong foo"
      lift $ MaybeT $ return Nothing

bar' :: ReaderT Int (MaybeT IO) String
bar' = do
  t <- ask
  f <- liftIO getLine
  return f
  if (length f) > t then
    return f
  else
    do
      liftIO $ putStrLn "wrong bar"
      lift $ MaybeT $ return Nothing
