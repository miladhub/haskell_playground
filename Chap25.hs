{-# LANGUAGE InstanceSigs #-}

module Chap25 where

import Control.Applicative

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g)
    => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = undefined
  
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) =
    let fgab = (<*>) <$> f 
    in Compose $ fgab <*> a

instance (Foldable f, Foldable g)
    => Foldable (Compose f g) where
  foldMap am (Compose fga) =
    foldMap (foldMap am) fga


instance (Traversable f', Traversable g)
    => Traversable (Compose f' g) where
  traverse :: (Applicative f) => (a -> f b) -> Compose f' g a -> f (Compose f' g b)
  traverse afb (Compose fga) =
    let ga_fgb = traverse afb
        ff'_gb = traverse ga_fgb fga
    in Compose <$> ff'_gb

{-
 - traverse :: (Applicative f, Traversable t) => (a -> f b) -> t  a -> f (t  b)
 -                                          f':  ((g a) -> f (g b)) -> f' (g a) -> f (f' (g b))
 -                                           g:  (a -> f b) -> g  a -> f (g  b)
 -}

{-
instance (Monad f, Monad g)
    => Monad (Compose f g) where
  return = pure
  (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
  (Compose a) >>= f =
    let x = (fmap . fmap) f a
    in undefined
-}
