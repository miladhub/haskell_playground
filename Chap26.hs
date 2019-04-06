module Chap26 where

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) =
    MaybeT $ (fmap . fmap) f ma

{-
  m (Maybe (a -> b)) ... m (Maybe a)
  
  Maybe (a -> b) <*> Maybe a -> Maybe b
  (<*>) Maybe (a -> b) :: Maybe a -> Maybe b
  (<*>) :: Maybe (a -> b) -> (Maybe a -> Maybe b)
  
  (<*>) <$> m (Maybe (a -> b)) -> m (Maybe a -> Maybe b)
  
  m (Maybe a -> Maybe b) <*> m (Maybe a) -> m (Maybe b)
-}

instance (Applicative m) => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (MaybeT fab) <*> (MaybeT mma) =
     let mab = (<*>) <$> fab
     in MaybeT $ mab <*> mma
