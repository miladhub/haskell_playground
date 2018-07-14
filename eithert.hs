module Either2 where

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) =
    let meb = (fmap . fmap) f mea
    in EitherT meb

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ pure (Right a)
  (EitherT emf) <*> (EitherT ema) = EitherT $ fmap (<*>) emf <*> ema

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ema) >>= f = EitherT $ do
    ea <- ema
    case ea of
      Left e -> return (Left e)
      Right a -> runEitherT (f a)

{-
let e = EitherT $ [Right 1, Left "foo"]
let f = EitherT $ [Right (+1)]
runEitherT $ f <*> e
et = EitherT $ return $ Right (42 :: Int) :: EitherT String IO Int
-}

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

eitherT :: Monad m => (e -> m c) -> (a -> m c) -> EitherT e m a -> m c
eitherT emc amc (EitherT mea) = do
  ea <- mea
  either emc amc ea
