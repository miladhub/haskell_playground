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
    let x = (<*>) <$> f 
    in Compose $ x <*> a

instance (Foldable f, Foldable g) =>
    Foldable (Compose f g) where
  foldMap am (Compose fga) =
    foldMap (foldMap am) fga
