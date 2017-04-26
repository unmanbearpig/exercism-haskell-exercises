module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

import Data.List (foldl')

data BST a = Node (BST a) a (BST a) | BSTNothing
  deriving (Eq, Show)

forNode :: (BST a -> b) -> BST a -> Maybe b
forNode _ BSTNothing = Nothing
forNode f x = Just $ f x

bstLeft :: BST a -> Maybe (BST a)
bstLeft = forNode $ \(Node left _ _) -> left

bstRight :: BST a -> Maybe (BST a)
bstRight = forNode $ \(Node _ _ right) -> right

bstValue :: BST a -> Maybe a
bstValue = forNode $ \(Node _ v _) -> v

empty :: BST a
empty = BSTNothing

fromList :: Ord a => [a] -> BST a
fromList = foldl' (flip insert) BSTNothing

insert :: Ord a => a -> BST a -> BST a
insert x BSTNothing = singleton x
insert x (Node left y right) =
  if x <= y
  then Node (insert x left) y right
  else Node left y (insert x right)

singleton :: a -> BST a
singleton x = Node BSTNothing x BSTNothing

toList :: BST a -> [a]
toList BSTNothing = []
toList (Node left x right) = toList left ++ x:toList right
