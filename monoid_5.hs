{-# LANGUAGE ScopedTypeVariables #-}

module MyMonoid6 where

import Data.Monoid hiding ((<>))
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

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine $ const mempty
  mappend = (<>)

instance (Show a, Show b, Function a, CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = 
    let fun = arbitrary :: Gen (Fun a b)
        fab = apply <$> fun :: Gen (a -> b)
        cab = Combine <$> fab
    in cab 

semigroupAssoc :: (Semigroup b, Eq b) =>
  a -> (Combine a b) -> (Combine a b) -> (Combine a b) -> Bool
semigroupAssoc a' a b c =
  let left = a <> (b <> c)
      right = (a <> b) <> c
  in (unCombine left) a' == (unCombine right) a'

monoidLeftIdentity :: (Semigroup b, Eq b, Monoid b) => a -> (Combine a b) -> Bool
monoidLeftIdentity a' cab = (unCombine (mempty `mappend` cab)) a' == (unCombine cab) a'

monoidRightIdentity :: (Semigroup b, Eq b, Monoid b) => a -> (Combine a b) -> Bool
monoidRightIdentity a' cab = (unCombine (cab `mappend` mempty)) a' == (unCombine cab) a'

main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: String -> Combine String String -> Combine String String -> Combine String String -> Bool)
  quickCheck (mli :: String -> Combine String String -> Bool)
  quickCheck (mlr :: String -> Combine String String -> Bool)

