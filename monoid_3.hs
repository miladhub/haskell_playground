{-# LANGUAGE ScopedTypeVariables #-}

module MyMonoid2 where

import Data.Monoid hiding ((<>))
import Data.Semigroup
import Test.QuickCheck

newtype BoolConj =
  BoolConj Bool
  deriving (Show, Eq)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = frequency [(1, return $ BoolConj True), (1, return $ BoolConj False)]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

type MyAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: MyAssoc)
  quickCheck (mli :: BoolConj -> Bool)
  quickCheck (mlr :: BoolConj -> Bool)

