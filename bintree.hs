module BinTrees where

data BinTree a =
  Node a (BinTree a) (BinTree a)
  | Nil
  deriving (Show, Eq)

leaf :: a -> BinTree a
leaf a = Node a Nil Nil

inverse :: BinTree a -> BinTree a
inverse Nil = Nil
inverse (Node a l r) = Node a (inverse r) (inverse l)
