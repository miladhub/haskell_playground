{-# LANGUAGE ScopedTypeVariables #-}

module MyMonoid7 where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Function

newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }

instance Show (Mem s a) where
  show _ = "Mem"

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend m1 m2 = Mem $ \s ->
    let (a1, s1) = runMem m1 $ s
        (a2, s2) = runMem m2 $ s1
    in (a1 <> a2, s2)

instance (Function s, CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary =
    let fun = arbitrary :: Gen (Fun s (a,s))
        f = apply <$> fun :: Gen (s -> (a,s))
    in Mem <$> f

semigroupAssoc :: (Eq a, Eq s, Monoid a) =>
  s -> (Mem s a) -> (Mem s a) -> (Mem s a) -> Bool
semigroupAssoc s m1 m2 m3 =
  let left = m1 <> (m2 <> m3)
      right = (m1 <> m2) <> m3
  in (runMem left) s == (runMem right) s

type MyAssoc = Int -> Mem Int String -> Mem Int String -> Mem Int String -> Bool

monoidLeftIdentity :: (Eq a, Eq s, Monoid a) => s -> (Mem s a) -> Bool
monoidLeftIdentity s m = (runMem (mempty `mappend` m)) s == (runMem m) s

monoidRightIdentity :: (Eq a, Eq s, Monoid a) => s -> (Mem s a) -> Bool
monoidRightIdentity s m = (runMem (m `mappend` mempty)) s == (runMem m) s

main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: MyAssoc)
  quickCheck (mli :: Int -> Mem Int String -> Bool)
  quickCheck (mlr :: Int -> Mem Int String -> Bool)


