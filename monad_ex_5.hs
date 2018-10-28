module MonadEx5 where

import Control.Applicative
import Control.Monad

j :: Monad m => m (m a) -> m a
j mma = mma >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh as f =
  let mbs = fmap f as
  in foldr myfold (return []) mbs

myfold :: Monad m => m a -> m [a] -> m [a]
myfold ma mas = do
  mas >>= \as ->
    ma >>= \a ->
      return (a : as)

flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas id
