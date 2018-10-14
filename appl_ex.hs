module ApplEx where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (Pair fa fa') <*> (Pair a a') = Pair (fa a) (fa' a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

main :: IO ()
main =
  let xs = Pair ("b", "w", 1 :: Int) ("b", "w", 1 :: Int)
  in quickBatch $ applicative xs

