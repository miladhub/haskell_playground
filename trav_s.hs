{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a =
  S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance Foldable (S n) where
  foldr f z (S na a) = f a z

instance ( Functor n, Arbitrary (n a), Arbitrary a ) => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary

instance ( Applicative n, Testable (n Property), EqProp a ) => EqProp (S n a) where
  (S x y) =-= (S p q) =
    (property $ (=-=) <$> x <*> p) .&. (y =-= q)

instance Traversable n => Traversable (S n) where
  traverse = undefined

main :: IO ()
main =
  let xs = undefined :: S [] (Int, Int, [Int])
  in do
    --sample' (arbitrary :: Gen (S [] Int))
    quickBatch $ functor xs
    --quickBatch $ traversable xs
