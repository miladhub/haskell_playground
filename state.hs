module MyState where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ \s ->
    let (a, s') = g s
    in (f a, s')

instance Applicative (Moi s) where
  pure a = Moi $ \s -> (a, s)
  Moi f <*> Moi g = Moi $ \s ->
    let (fab, s') = f s
        (a, s'') = g s'
    in (fab a, s'')

instance Monad (Moi s) where
  return = pure
  Moi f >>= g = Moi $ \s ->
    let (a, s') = f s
        x = g a
        y = (runMoi x)
    in y s'
