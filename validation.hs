{-# LANGUAGE ScopedTypeVariables #-}

module MyValidation where

import Data.Semigroup
import Test.QuickCheck

data Validation a b =
  Fail a | Succ b 
  deriving (Eq, Show)

instance Semigroup a =>
  Semigroup (Validation a b) where
  (Succ b) <> _ = Succ b
  (Fail a') <> (Fail a'') = Fail $ a' <> a''
  (Fail a) <> (Succ b) = Succ b

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Validation a b) where
  arbitrary =
    let gena = arbitrary :: Gen a
        genb = arbitrary :: Gen b
    in frequency [(1, Fail <$> gena), (1, Succ <$> genb)]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type MyAssoc = (Validation String String) -> (Validation String String) -> (Validation String String) -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: MyAssoc)
