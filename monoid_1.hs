{-# LANGUAGE ScopedTypeVariables #-}

module MyMonoid1 where

import Data.Monoid hiding ((<>))
import Data.Semigroup
import Test.QuickCheck

newtype Identity a =
  Identity a
  deriving (Show, Eq)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity a') <> (Identity a'') = Identity (a' <> a'')

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

type MyAssoc =
  Identity String -> Identity String -> Identity String -> Bool

main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: MyAssoc)
  quickCheck (mli :: Identity String -> Bool)
  quickCheck (mlr :: Identity String -> Bool)

