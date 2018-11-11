module Folds where

import Data.Monoid

mysum :: (Foldable t, Num a) => t a -> a
--mysum = foldr (+) 0
mysum = getSum . foldMap Sum

myproduct :: (Foldable t, Num a) => t a -> a
--myproduct = foldr (*) 1
myproduct = getProduct . foldMap Product

mymin :: (Foldable t, Ord a) => t a -> Maybe a
mymin = foldr foo Nothing

foo :: Ord a => a -> Maybe a -> Maybe a
foo a (Just b) = Just (min a b)
foo a _ = Just a

mymax :: (Foldable t, Ord a) => t a -> Maybe a
mymax = foldr bar Nothing

bar :: Ord a => a -> Maybe a -> Maybe a
bar a (Just b) = Just (max a b)
bar a _ = Just a

mynull :: (Foldable t) => t a -> Bool
mynull = foldr baz True

baz :: a -> Bool -> Bool
baz _ _ = False

mylength :: (Foldable t) => t a -> Int
mylength = mysum . myones

myones :: (Foldable t) => t a -> [Int]
myones = foldr (\_ l -> l ++ [1]) []

myToList :: (Foldable t) => t a -> [a]
myToList = foldr (\a l -> a : l) []

myfold :: (Foldable t, Monoid m) => t m -> m
myfold = foldMap id

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap f = foldr (\a m -> f a <> m) mempty

data Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr f z (Constant b) = f b z

data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f z (Two a b) = f b z

data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f z (Three a b c) = f c z

data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldr f z (Three' a _ b) = f b z

data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldr f z (Four' a _ _ b) = f b z


filterF :: (Applicative f, Foldable t, Monoid (f a))
  => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)
