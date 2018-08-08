{-# LANGUAGE ScopedTypeVariables #-}

module MySemigroup where

import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Function

data Combine a b =
  Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show _ = "<combine>"

instance (Semigroup b) => Semigroup (Combine a b) where
  cab' <> cab'' =
    let ab' = unCombine cab'
        ab'' = unCombine cab''
    in Combine $ \n ->
      (ab' n) <> (ab'' n)

instance (Show a, Show b, Function a, CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = 
    let fun = arbitrary :: Gen (Fun a b)
        fab = apply <$> fun :: Gen (a -> b)
        cab = Combine <$> fab
    in cab 

{-
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)
-}

semigroupAssoc :: (Semigroup b, Eq b) =>
  a -> (Combine a b) -> (Combine a b) -> (Combine a b) -> Bool
semigroupAssoc a' a b c =
  let left = a <> (b <> c)
      right = (a <> b) <> c
  in (unCombine left) a' == (unCombine right) a'

type MyAssoc = String -> Combine String String -> Combine String String -> Combine String String -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: MyAssoc)

