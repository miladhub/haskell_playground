module XMonadMonoid where

import Data.Monoid

data Foo = Foo { foo :: Int -> Sum Int }

f = Foo { foo = \i -> Sum (i + 1) }

thefoo :: Int -> Sum Int
thefoo = foo f

sum42 :: Sum Int
sum42 = Sum 42

g = f { foo = \i -> (sum42 `mappend` thefoo i) }

h = f { foo = (\i -> sum42) `mappend` thefoo }

