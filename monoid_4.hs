{-# LANGUAGE ScopedTypeVariables #-}

module MyMonoid4 where

import Data.Monoid hiding ((<>))
import Data.Semigroup
import Test.QuickCheck

newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _ <> (BoolDisj True) = BoolDisj True
  _ <> _ = BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = frequency [(1, return $ BoolDisj True), (1, return $ BoolDisj False)]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

type MyAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool

main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: MyAssoc)
  quickCheck (mli :: BoolDisj -> Bool)
  quickCheck (mlr :: BoolDisj -> Bool)

