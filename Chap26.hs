module Chap26 where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

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

{-
 m (Either e (a -> b)) ... m (Either e a) ~> m (Either e b)
 (Either e (a -> b)) <*> (Either e a) ~> (Either e b)

-}
instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ (pure . pure) a
  (EitherT meab) <*> (EitherT mea) =
    let x = (<*>) <$> meab
        y = x <*> mea
    in EitherT $ y

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT meab) >>= f = EitherT $ do -- m (Either e b)
    eab <- meab
    case eab of
      Left b  -> return (Left b)
      Right a -> runEitherT (f a)

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea

swapEither :: Either e a -> Either a e
swapEither ea =
  case ea of
    Left e  -> Right e
    Right a -> Left a

{-
 either :: (a -> c) -> (b -> c) -> Either a b -> c
-}
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mea) =
  let x = fmap (either f g) mea
  in x >>= id 

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) =
    ReaderT $ (fmap f) . rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT $ \x -> pure a
  (ReaderT rmf) <*> (ReaderT rma) =
    let x = \r -> (rmf r) <*> (rma r)
    in ReaderT x

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f =
    let x = \r -> (fmap f) $ (rma r)
        y = (fmap . fmap) runReaderT x
        z = \r -> y r <*> (return r)
        w = fmap (>>= id) z
    in ReaderT w

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT sma) = 
    let x = (fmap . fmap) g sma -- getting to (a, s)
        g = \(a, s) -> (f a, s)
    in StateT x

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)
  (StateT smf) <*> (StateT sma) = -- StateT $ s -> m (b, s)
    StateT $ \s -> do
      (f, s1) <- smf s
      (a, s2) <- sma s1
      return (f a, s2)

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s1) <- sma s
    (b, s2) <- runStateT (f a) $ s1
    return (b, s2)

{-
newtype EitherT e m a =
  EitherT { runExceptT :: m (Either e a) }

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }
-}

embedded :: MaybeT (EitherT String (ReaderT () IO)) Int
embedded = 
  let x = const (Right (Just 1)) -- x :: () -> Either String (Maybe Integer)
      w = return <$> x           -- w :: () -> IO (Either String (Maybe Integer))
      z = ReaderT $ w            -- z :: ReaderT () IO (Either String (Maybe Integer))
      y = EitherT $ z            -- y :: EitherT String (ReaderT () IO) (Maybe Integer)
 in MaybeT y

embedded' :: MaybeT (EitherT String (ReaderT () IO)) Int
embedded' =
  MaybeT $ EitherT $ ReaderT $ return <$> (const (Right (Just 1)))

instance MonadTrans MaybeT where
  lift ma = MaybeT $ do
    a <- ma
    return (Just a)

instance MonadTrans (EitherT e) where
  lift ma = EitherT $ (fmap Right ma)

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)

instance MonadTrans (ReaderT r) where
  lift ma = ReaderT $ \r -> do
    a <- ma
    return a

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO
