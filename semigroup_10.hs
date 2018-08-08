{-# LANGUAGE ScopedTypeVariables #-}

module MySemigroup where

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

instance (Show a, Function a, CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = 
    let fun = arbitrary :: Gen (Fun a a)
        faa = apply <$> fun :: Gen (a -> a)
        caa = Comp <$> faa
    in caa

semigroupAssoc :: (Semigroup a, Eq a) =>
  a -> (Comp a) -> (Comp a) -> (Comp a) -> Bool
semigroupAssoc a' a b c =
  let left = a <> (b <> c)
      right = (a <> b) <> c
  in (unComp left) a' == (unComp right) a'

type MyAssoc = String -> Comp String -> Comp String -> Comp String -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: MyAssoc)

