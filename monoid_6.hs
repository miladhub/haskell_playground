{-# LANGUAGE ScopedTypeVariables #-}

module MyMonoid7 where

import Data.Monoid hiding ((<>))
import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Function

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  show _ = "<comp>"

instance Semigroup (Comp a) where
  ca' <> ca'' =
    let ab' = unComp ca'
        ab'' = unComp ca''
    in Comp $ ab' . ab''

instance Monoid (Comp a) where
  mempty = Comp $ id
  mappend = (<>)

instance (Show a, Function a, CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = 
    let fun = arbitrary :: Gen (Fun a a)
        faa = apply <$> fun :: Gen (a -> a)
        caa = Comp <$> faa
    in caa

semigroupAssoc :: (Eq a) =>
  a -> (Comp a) -> (Comp a) -> (Comp a) -> Bool
semigroupAssoc a' a b c =
  let left = a <> (b <> c)
      right = (a <> b) <> c
  in (unComp left) a' == (unComp right) a'

type MyAssoc = String -> Comp String -> Comp String -> Comp String -> Bool

monoidLeftIdentity :: (Eq a) => a -> (Comp a) -> Bool
monoidLeftIdentity a' cab = (unComp (mempty `mappend` cab)) a' == (unComp cab) a'

monoidRightIdentity :: (Eq a) => a -> (Comp a) -> Bool
monoidRightIdentity a' cab = (unComp (cab `mappend` mempty)) a' == (unComp cab) a'

main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: MyAssoc)
  quickCheck (mli :: String -> Comp String -> Bool)
  quickCheck (mlr :: String -> Comp String -> Bool)

