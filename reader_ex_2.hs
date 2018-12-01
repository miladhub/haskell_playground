module MyReaderEx2 where

import Control.Applicative

newtype Reader r a =
  Reader { runReader :: r -> a }

myLiftA2 :: Applicative f =>
  (a -> b -> c)
  -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader
