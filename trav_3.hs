module Trav3 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Optional a = Nada | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap f (Yep a) = Yep $ f a
  fmap _ _ = Nada

instance Foldable Optional where
  foldr f z (Yep a) = f a z
  foldr _ z Nada    = z

instance Traversable Optional where
  sequenceA (Yep a) = Yep <$> a
  sequenceA Nada    = pure Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary =
    frequency [
      (1, Yep <$> arbitrary),
      (1, return Nada)
    ]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

main :: IO ()
main =
  let xs = undefined :: Optional (Int, Int, [Int])
  in do
    quickBatch $ functor xs
    quickBatch $ traversable xs
