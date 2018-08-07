module MySemigroup where

import Data.Semigroup
import Test.QuickCheck

newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _ <> (BoolDisj True) = BoolDisj True
  _ <> _ = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = frequency [(1, return $ BoolDisj True), (1, return $ BoolDisj False)]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type OneAssoc = BoolDisj
type ThreeAssoc = OneAssoc -> OneAssoc -> OneAssoc -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: ThreeAssoc)

