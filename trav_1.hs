module Trav1 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a 
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldr f z (Identity a) = f a z

instance Traversable Identity where
  sequenceA (Identity fa) = Identity <$> fa

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

main :: IO ()
main =
  let xs = undefined :: Identity (Int, Int, [Int])
  in do
    quickBatch $ functor xs
    quickBatch $ traversable xs
