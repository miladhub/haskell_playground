module App where

import People
import Chap26

match :: String -> String -> IO (Either String Integer)
match pn qn = do
  mp <- findPerson pn
  mq <- findPerson qn
  return $ case mp of
    Just p -> case mq of
      Just q -> Right $ (age p + age q)
      Nothing -> Left $ "No such person: " ++ qn
    Nothing -> Left $ "No such person: " ++ pn

match' :: String -> String -> IO (Maybe Integer)
match' pn qn = runMaybeT $ do
  p <- MaybeT $ findPerson pn
  q <- MaybeT $ findPerson qn
  return $ (age p + age q)
