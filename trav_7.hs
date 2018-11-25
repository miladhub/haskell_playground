module Trav7 where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Big a b =
  Big a b b
  deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big a b b') = f b <> f b'

instance Traversable (Big a) where
  sequenceA (Big a b b') = Big a <$> b <*> b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary =
    Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

main :: IO ()
main =
  let xs = undefined :: Big Int (Int, Int, [Int])
  in do
    quickBatch $ functor xs
    quickBatch $ traversable xs
