data Two a b =
  Two a b
  deriving (Eq, Show)

data Or a b =
    First a
  | Second b
  deriving (Eq, Show)
  
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)
  
newtype Identity a = Identity a
  deriving Show
  
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)
  
data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
  
data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)
  
data Four a b c d = Four a b c d deriving (Eq, Show)
data Four' a b = Four' a a a b deriving (Eq, Show)

data Trivial = Trivial deriving (Eq, Show)
