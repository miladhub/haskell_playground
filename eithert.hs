newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }
  
instance Functor m
      => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea
  
instance Applicative m
      => Applicative (EitherT e m) where
  pure a = EitherT $ pure (Right a)
  (EitherT mef) <*> (EitherT mea) = EitherT $ fmap (<*>) mef <*> mea
  
{-
  mef = m (Either e a->b)
  mea = m (Either e a)
  (<*>) (Either e a->b) = (Either e a) -> (Either e b)
  fmap (<*>) (m (Either e a->b)) = m ( (Either e a) -> (Either e b) )
  fmap (<*>) mef = m ( (Either e a) -> (Either e b) )
  m ( (Either e a) -> (Either e b) ) <*> m (Either e a) = m (Either e b)
  fmap (<*>) mef <*> mea
-}
{-
*Main> let l = [Right (+1)]
*Main> let e = EitherT l
*Main> runEitherT $ e <*> EitherT [Right 1]
[Right 2]
-}