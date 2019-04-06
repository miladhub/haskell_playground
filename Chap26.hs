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

{-
 m (Maybe a) ... (a -> (MaybeT m b)) ...->... Maybe T $ m (Maybe b)
 g :: (Maybe a) -> m (Maybe b)
 f <$> mma -> m (Maybe (MaybeT m b))
 m (Maybe (m (Maybe m b))
-}
instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (MaybeT mma) >>= f = MaybeT $ do -- needs a m (Maybe b)
    ma <- mma -- m (Maybe a) -> Maybe a
    case ma of
      Nothing -> return Nothing -- wraps the Nothing into m (Nothing) 
      Just x  -> runMaybeT (f x) -- x :: a, f x :: MaybeT (m (Maybe b)), after run becomes m (Maybe b)

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure = undefined
  f <*> a = undefined

