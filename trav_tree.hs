module Trav8 where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Tree a =
  Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap f Empty = mempty 
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = (foldMap f l) <> (f a) <> (foldMap f r)

instance Traversable Tree where
  sequenceA Empty = pure Empty 
  sequenceA (Leaf a) = Leaf <$> a
  sequenceA (Node l a r) = Node <$> sequenceA l <*> a <*> sequenceA r

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    frequency [
      (1, return Empty),
      (1, Leaf <$> arbitrary),
      (1, Node <$> arbitrary <*> arbitrary <*> arbitrary)
    ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

main :: IO ()
main =
  let xs = undefined :: Tree (Int, Int, [Int])
  in do
    quickBatch $ functor xs
    quickBatch $ traversable xs
