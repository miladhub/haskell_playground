{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a =
  S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S n a) = foldMap f n <> f a

instance ( Functor n, Arbitrary (n a), Arbitrary a ) => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

{-
This doesn't work.
instance ( Applicative n, Testable (n Property), EqProp a ) => EqProp (S n a) where
  (S x y) =-= (S p q) =
    (property $ (=-=) <$> x <*> p) .&. (y =-= q)
-}

instance Traversable n => Traversable (S n) where
  traverse f (S na a) =
    let fnb = traverse f na
        fb = f a
        snb = S <$> fnb <*> fb
    in snb

main :: IO ()
main =
  let xs = undefined :: S [] (Int, Int, [Int])
  in do
    quickBatch $ functor xs
    quickBatch $ traversable xs
