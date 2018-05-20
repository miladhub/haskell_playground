newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity a) (Identity a') = Identity (a a')
  
{--
*Main> pure (*) <*> Identity 5 <*> Identity 3
Identity 15
*Main> (*) <$> Identity 5 <*> Identity 3
Identity 15

import Data.Monoid
--}
  
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a
      => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant a) (Constant a') = Constant (mappend a a')
  
{--
Prelude> let f = Constant (Sum 1)
Prelude> let g = Constant (Sum 2)
Prelude> f <*> g
Constant {getConstant = Sum {getSum = 3}
Prelude> Constant undefined <*> g
Constant (Sum {getSum =
  *** Exception: Prelude.undefined
Prelude> pure 1
1
Prelude> pure 1 :: Constant String Int
Constant {getConstant = ""}
--}