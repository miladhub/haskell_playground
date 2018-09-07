{-# LANGUAGE FlexibleInstances #-}

module FunkFlip where

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K a b =
  K a

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap ff (LiftItOut fa) = LiftItOut $ fmap ff fa

data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap ff (DaWrappa fa ga) = DaWrappa (fmap ff fa) (fmap ff ga)

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap ff (IgnoringSomething fa gb) = IgnoringSomething fa (fmap ff gb)

data Notorious g o a t =
  Notorious (g o) (g a) (g t)
  deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap ff (Notorious go ga gt) = Notorious go ga (fmap ff gt)

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

data GoatLord a =
  NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats gl gl' gl'') = MoreGoats (fmap f gl) (fmap f gl') (fmap f gl'')

data TalkToMe a =
  Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print s a) = Print s $ f a
  fmap f (Read fsa) = Read $ f . fsa



